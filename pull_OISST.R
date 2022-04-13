# using same source data as SOE, https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html
# add text below to documentation
# "NOAA High Resolution SST data provided by the NOAA/OAR/ESRL PSL, Boulder, Colorado, USA, from their Web site" [above]
# initial pull code kindly provided by Kim Bastille
# https://github.com/kimberly-bastille/ecopull/blob/main/.github/workflows/pull_satellite_data.yml
# pulling daily gridded SST for each year 1985-2021 using her code starting line 260

# also use Kim's nc_to_raster function for NEUS shelf
# from https://github.com/kimberly-bastille/ecopull/blob/main/R/utils.R

#' Convert netcdf to raster
#'
#' This function converts a netcdf object to a raster object
#' @param nc The nc file path
#' @param varname The name of the variable to convert to a raster
#' @param extent The latitude and longitude range the data should have, of the form c(xmin, xmax, ymin, ymax). Defaults to `c(0, 360, -90, 90)`
#' @param crop An extent object to use to crop data. Defaults to `raster::extent(280, 300, 30, 50)` (Northeast US)
#' @param show_images Boolean. Whether to display images of the data. Useful to check if cropping is occurring correctly.
#' @return A raster brick
#' @importFrom magrittr %>%
#' @export

nc_to_raster <- function(nc,
                         varname,
                         extent = c(0, 360, -90, 90),
                         crop = raster::extent(280, 300, 30, 50),
                         show_images = FALSE) {
  
  message("Reading .nc as brick...")
  
  r <- raster::brick(nc, varname = varname)
  
  message("Setting CRS...")
  raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)
  
  if(show_images){
    par(mfrow = c(1,2))
    raster::plot(r, 1, sub = "Full dataset")
  }
  
  message("Cropping data...")
  ne_data <- raster::crop(r, crop)
  #ne_data <- raster::rotate(ne_data) add here for future pulls
  
  if(show_images){
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1,1))
  }
  
  message("Done!")
  
  return(ne_data)
}

# pull the data and store NEUS rasters
# adapted from https://github.com/kimberly-bastille/ecopull/blob/main/.github/workflows/run_recent_years.yaml

varname <- "sst"

years <- 2021
for(i in years) {
  name <- paste0(i, ".nc")
  dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
  filename <- here::here("data-raw","gridded", "sst_data", paste0("test_", i, ".grd"))
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", i, ".v2.nc")
  download.file(url, destfile = name)
  
  text <- knitr::knit_expand(text = "test_{{year}} <- nc_to_raster(nc = name, varname = varname)
                                     raster::writeRaster(test_{{year}}, filename = filename, overwrite=TRUE)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
  unlink(name) # remove nc file to save space
  print(paste("finished",i))
}

# function to make rasters into data frame for merge with survey
# needs long df with date split to year, month, day, lat, lon, sst
# crop to NEUS extent
# from https://towardsdatascience.com/transforming-spatial-data-to-tabular-data-in-r-4dab139f311f

raster_to_sstdf <- function(brick,
                            rotate=TRUE){
  
  if(rotate) brick_r <- raster::rotate(brick)
  brick_r <- raster::crop(brick_r, raster::extent(-77,-65,35,45))
  sstdf <- as.data.frame(raster::rasterToPoints(brick_r, spatial = TRUE))
  sstdf <- sstdf %>%
    dplyr::rename(Lon = x,
                  Lat = y) %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = c("year", "month", "day"),
                        names_prefix = "X",
                        names_sep = "\\.",
                        values_to = "sst",
    )
  return(sstdf)
}

# working with the rasters in memory, could read in files instead in name statement
years <- 1985:2021
for(i in years) {
  name <- get(paste0("test_",i))
  filename <- here::here("data-raw","gridded", "sst_data", paste0("sst", i, ".rds"))
  text <- knitr::knit_expand(text = "sst{{year}} <- raster_to_sstdf(brick = name)
                                     saveRDS(sst{{year}}, filename)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
}

#visualize
library(dplyr)
library(ggplot2)
library(FishStatsUtils)

oneday <- sst2021[sst2021$month=="07" & sst2021$day=="04",] 

jan15 <- sst2021[sst2021$month=="01" & sst2021$day=="15",] 

mar15 <- sst2021[sst2021$month=="03" & sst2021$day=="15",]

may15 <- sst2021[sst2021$month=="05" & sst2021$day=="15",]

jul15 <- sst2021[sst2021$month=="07" & sst2021$day=="15",]

sep15 <- sst2021[sst2021$month=="09" & sst2021$day=="15",]

nov15 <- sst2021[sst2021$month=="11" & sst2021$day=="15",]

dailysstplot <- function(oneday){
  ggplot() +
    geom_tile(data = oneday, aes(x = Lon, y = Lat, fill = sst)) +
    geom_sf(data = ecodata::coast) +
    geom_point(data = northwest_atlantic_grid, aes(x = Lon, y = Lat), size=0.05, alpha=0.1) +
    scale_fill_gradientn(name = "Temp C",
                         limits = c(0.5, 31),
                         colours = c(scales::muted("blue"), "white",
                                     scales::muted("red"), "black")) +
    coord_sf(xlim = c(-77, -65), ylim = c(35, 45)) + 
    ggtitle(paste("SST, mm dd yyyy:", unique(oneday$month),
                   unique(oneday$day), unique(oneday$year), sep = " "))
}

dailysstplot(oneday)

par(mfrow=c(2,3))
dailysstplot(jan15)
dailysstplot(mar15)
dailysstplot(may15)
dailysstplot(jul15)
dailysstplot(sep15)
dailysstplot(nov15)