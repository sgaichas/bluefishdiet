# VAST attempt 1 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization
# and https://github.com/NOAA-EDAB/neusvast/blob/master/analysis/models/pisc_pesc.R

# may need to downgrade TMB to VAST Matrix package version?
# see https://github.com/James-Thorson-NOAA/VAST/issues/289

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
  select(Catch_g = meanbluepywt,
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
  select(Catch_g = meanbluepywt,
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
settings = make_settings( n_x = 1000, 
                          Region = "northwest_atlantic",
                          strata.limits = list('All_areas' = 1:1e5), 
                          purpose = "index2", 
                          bias.correct = FALSE,
                          use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          FieldConfig = c(Omega1 = "IID", 
                                          Epsilon1 = "IID", 
                                          Omega2 = "IID", 
                                          Epsilon2 = "IID")
                          )

 #Aniso=FALSE, #correct ln_H_input at bound
 #FieldConfig["Epsilon1"]=0, #correct L_epsilon1_z approaching 0  
 #FieldConfig["Epsilon2"]=0 #correct L_epsilon2_z approaching 0 
 #increase knots to address bounds for logkappa?
 # https://github.com/James-Thorson-NOAA/VAST/issues/300
 # or try finescale=FALSE
 # then Omegas hit bounds, had to turn then off too

# Run model fall
fit = fit_model( settings = settings, 
                 Lat_i = bluepyagg_stn_fall[,'Lat'], 
                 Lon_i = bluepyagg_stn_fall[,'Lon'], 
                 t_i = bluepyagg_stn_fall[,'Year'], 
                 b_i = as_units(bluepyagg_stn_fall[,'Catch_g'], 'g'), 
                 a_i = as_units(bluepyagg_stn_fall[,'AreaSwept_km2'], 'km^2'),
                 working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

# Run model spring

season <- c("spring")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

settings = make_settings( n_x = 1000, 
                          Region = "northwest_atlantic",
                          strata.limits = list('All_areas' = 1:1e5), 
                          purpose = "index2", 
                          bias.correct = FALSE,
                          use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          FieldConfig = c(Omega1 = "IID", 
                                          Epsilon1 = "IID", 
                                          Omega2 = "IID", 
                                          Epsilon2 = "IID")
                         )
                          

fit = fit_model( settings = settings, 
                 Lat_i = bluepyagg_stn_spring[,'Lat'], 
                 Lon_i = bluepyagg_stn_spring[,'Lon'], 
                 t_i = bluepyagg_stn_spring[,'Year'], 
                 b_i = as_units(bluepyagg_stn_spring[,'Catch_g'], 'g'), 
                 a_i = as_units(bluepyagg_stn_spring[,'AreaSwept_km2'], 'km^2'),
                 working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 
