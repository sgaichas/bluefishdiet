# using higher resolution source data than the SOE, https://www.ncei.noaa.gov/products/avhrr-pathfinder-sst
# add citation text below to documentation
# "These data were provided by GHRSST and the NOAA National Centers 
#  for Environmental Information."
#
# "Saha, Korak; Zhao, Xuepeng; Zhang, Huai-min; Casey, Kenneth S.; Zhang, Dexin; 
#  Baker-Yeboah, Sheekela; Kilpatrick, Katherine A.; Evans, Robert H.; Ryan, Thomas; 
#  Relph, John M. (2018). AVHRR Pathfinder version 5.3 level 3 collated (L3C) 
#  global 4km sea surface temperature for 1981-Present. [indicate subset used]. 
#  NOAA National Centers for Environmental Information. Dataset. 
#  https://doi.org/10.7289/v52j68xx. Accessed [date].
#
# I ran this in SSTmethods.Rmd but this is the code
# 
#
#read in diet data with month day fields
bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

stations <- bluepyagg_stn_all %>%
  dplyr::mutate(day = str_pad(day, 2, pad='0'),
                month = str_pad(month, 2, pad='0'),
                yrmody = as.numeric(paste0(year, month, day))) %>%
  dplyr::select(id, declon, declat, year, yrmody) %>%
  na.omit() %>%
  sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE) 

#full list of filenames to filter by survey dates
# get a list from https server R: https://stackoverflow.com/questions/67695769/list-files-on-https-server-using-r


years <- 1985:2021
#years <- 2011:2021 #this was because I had to stop downloading, would have done all
for(i in years) {
  sstyrdf <- data.frame()
  url <- paste0("https://www.ncei.noaa.gov/data/oceans/pathfinder/Version5.3/L3C/", i, "/data/")
  page <- rvest::read_html(url)
  #list of SST AVHRR files in each year folder
  yrfiles <- rvest::html_elements(page, xpath= ".//a[contains(@href, '.nc')]") %>%
    rvest::html_text()
  
  # keep only bluefish dates in SST year, +/- 2 days for borrowing
  datesyr <- stations %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(year == i) %>%
    dplyr::select(yrmody) %>%
    dplyr::distinct() %>%
    dplyr::mutate(p1 = yrmody + 1,
                  p2 = yrmody + 2,
                  m1 = yrmody - 1,
                  m2 = yrmody - 2) %>%
    tidyr::pivot_longer(everything(), values_to = "yrmody") %>%
    dplyr::select(yrmody) %>%
    dplyr::distinct() 
  
  #remove bad dates and convert to character for comparison with filenames
  datesyrch <- lubridate::parse_date_time(datesyr$yrmody, "%y%m%d") %>%
    na.omit() %>%
    format("%Y%m%d")
  
  #list of files for the survey dates plus or minus 2 days 
  yrfiles <- yrfiles[stringr::str_starts(yrfiles, paste(datesyrch, collapse = "|"))]
  
  # get data from all files
  for(d in 1:length(yrfiles)){
    name <- paste0(stringr::str_extract(yrfiles[d], "^.{14}"), ".nc")
    download.file(paste0(url, yrfiles[d]), destfile = name)
    
    r <- terra::rast(name, c("sea_surface_temperature",
                             "quality_level"))
    rcrop <- terra::crop(r, c(-77,-65,35,45))
    sstdaydf <- terra::as.data.frame(rcrop, xy=TRUE) %>% 
      dplyr::mutate(sst = sea_surface_temperature - 273.15) %>%
      dplyr::filter(quality_level > 0) %>%
      dplyr::mutate(date = unique(terra::time(rcrop)))
    
    unlink(name) # remove nc file to save space
    
    sstyrdf <- bind_rows(sstyrdf, sstdaydf)
    
  }# end yrfiles within a year
  
  #save each year's sst dataframe for survey dates +-2
  filename <- here::here("data-raw","gridded", "sst_data", paste0("AVHRRsst_", i, ".rds"))
  saveRDS(sstyrdf, filename)
  
} # end years

# plotting function has night and day as facets, takes one day's data
plotAVHRR <- function(sstdfdat){
  ggplot() +
    geom_tile(data = sstdfdat, aes(x = x, y = y, fill = sst)) +
    geom_sf(data = ecodata::coast) +
    scale_fill_gradientn(name = "Temp C",
                         limits = c(0.5, 31),
                         colours = c(scales::muted("blue"), "white",
                                     scales::muted("red"), "black")) +
    coord_sf(xlim = c(-77, -65), ylim = c(35, 45)) +
    #ggtitle(paste("SST, AVHRR", unique(sstdfdat$date), sep = " ")) +
    facet_wrap(~as.character(date))
}

# example plots for 2021
# this takes a looooooong time
sstyrdf <- readRDS(here("data-raw/gridded/sst_data/AVHRRsst_2021.rds"))

survdates <- unique(stringr::str_extract(sstyrdf$date, "^.{10}"))

for(survday in survdates){
  
  mapday <- sstyrdf %>%
    filter(quality_level>2, #Kim Hyde recommendation
           str_detect(date, pattern = regex(paste0("\\b", survday))))
  
  cat("  \n####",  as.character(survday),"  \n")
  print(plotAVHRR(mapday)) 
  cat("  \n")   
  
}
