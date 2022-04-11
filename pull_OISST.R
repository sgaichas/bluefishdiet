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

years <- 1996:2021
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

