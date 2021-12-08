# VAST attempt 1 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization
# and https://github.com/NOAA-EDAB/neusvast/blob/master/analysis/models/pisc_pesc.R


# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

bluepyagg_stn <- readRDS(here("fhdat/bluepyagg_stn.rds"))

bluepyagg_stn_fall <- bluepyagg_stn %>%
  ungroup() %>%
  filter(season_ng == "FALL") %>%
  mutate(Vessel = "NEFSC",
         AreaSwept_km2 = 0.0384) %>%
  select(Catch_KG = meanbluepywt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon) %>%
  as.data.frame()

bluepyagg_stn_spring <- bluepyagg_stn %>%
  filter(season_ng == "SPRING")%>%
  mutate(Vessel = "NEFSC",
         AreaSwept_km2 = 0.0384) %>%
  select(Catch_KG = meanbluepywt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon)%>%
  as.data.frame()

# select dataset and set directory for output

season <- c("fall")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302
settings = make_settings( n_x = 100, 
                          Region = "northwest_atlantic",
                          strata.limits = list('All_areas' = 1:1e5), 
                          purpose = "index2", 
                          bias.correct = FALSE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = bluepyagg_stn_fall[,'Lat'], 
                 Lon_i = bluepyagg_stn_fall[,'Lon'], 
                 t_i = bluepyagg_stn_fall[,'Year'], 
                 b_i = bluepyagg_stn_fall[,'Catch_KG'], 
                 a_i = bluepyagg_stn_fall[,'AreaSwept_km2'], 
                 working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit )