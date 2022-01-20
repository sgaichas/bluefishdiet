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

#bluepyagg_stn <- bluepyagg_pred_stn

# filter to assessment years at Tony's suggestion
# try datasets with subset of predators (start with 1)

bluepyagg_stn_fall <- bluepyagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL",
         year > 1984) %>%
  mutate(Vessel = "NEFSC",
         #AreaSwept_km2 = 0.0384) %>%
         AreaSwept_km2 = 1, #Elizabeth's code
         declon = -declon) %>% 
  select(Catch_g = meanbluepywt, #use bluepywt for individuals input in example
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon) %>%
  na.omit() %>%
  as.data.frame()

bluepyagg_stn_spring <- bluepyagg_stn %>%
  filter(season_ng == "SPRING",
         year > 1984) %>%
  mutate(Vessel = "NEFSC",
         #AreaSwept_km2 = 0.0384) %>%
         AreaSwept_km2 =1, #Elizabeth's code
         declon = -declon) %>% 
  select(Catch_g = meanbluepywt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon)%>%
  na.omit() %>%
  as.data.frame()


# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302

# use only MAB, GB, GOM, SS EPUs 
# leave out south of Cape Hatteras at Elizabeth's suggestion
# could also leave out SS?
# CHECK if these EPUs match what we use in SOE

MABGBGOM <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Mid_Atlantic_Bight", "Georges_Bank", "Gulf_of_Maine")) %>% 
  select(MAB_GB_GOM = stratum_number) %>% 
  distinct()

MABGBGOMSS <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Mid_Atlantic_Bight", "Georges_Bank", "Gulf_of_Maine",
                    "Scotian_Shelf")) %>% 
  select(MAB_GB_GOM_SS = stratum_number) %>% 
  distinct()

MABGB <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Mid_Atlantic_Bight", "Georges_Bank")) %>% 
  select(MAB_GB_GOM_SS = stratum_number) %>% 
  distinct()

GB <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Georges_Bank")) %>% 
  select(MAB_GB_GOM_SS = stratum_number) %>% 
  distinct()


MAB <- northwest_atlantic_grid %>% 
  filter(EPU %in% c("Mid_Atlantic_Bight")) %>% 
  select(MAB_GB_GOM_SS = stratum_number) %>% 
  distinct()

# configs
FieldConfig <- c(
  "Omega1"   = 0,   # number of spatial variation factors (0, 1, AR1)
  "Epsilon1" = 0,   # number of spatio-temporal factors
  "Omega2"   = 0, 
  "Epsilon2" = 0
) 

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

strata.limits <- as.list(MABGBGOMSS)

settings = make_settings( n_x = 500, 
                          Region = "northwest_atlantic",
                          #strata.limits = list('All_areas' = 1:1e5), full area
                          strata.limits = strata.limits,
                          purpose = "index2", 
                          bias.correct = FALSE,
                          use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          FieldConfig = FieldConfig,
                          #RhoConfig = RhoConfig
                          )

 #Aniso=FALSE, #correct ln_H_input at bound
 #FieldConfig["Epsilon1"]=0, #correct L_epsilon1_z approaching 0  
 #FieldConfig["Epsilon2"]=0 #correct L_epsilon2_z approaching 0 
 #increase knots to address bounds for logkappa?
 # https://github.com/James-Thorson-NOAA/VAST/issues/300
 # or try finescale=FALSE
 # then Omegas hit bounds, had to turn then off too

# select dataset and set directory for output

season <- c("fall_500_noomeps12_noaniso")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

# Run model fall
# fit = fit_model( settings = settings, 
#                  Lat_i = bluepyagg_stn_fall[,'Lat'], 
#                  Lon_i = bluepyagg_stn_fall[,'Lon'], 
#                  t_i = bluepyagg_stn_fall[,'Year'], 
#                  b_i = as_units(bluepyagg_stn_fall[,'Catch_g'], 'g'), 
#                  a_i = as_units(bluepyagg_stn_fall[,'AreaSwept_km2'], 'km^2'),
#                  working_dir = paste0(working_dir, "/"))

fit <- fit_model(
  settings = settings, 
  Lat_i = bluepyagg_stn_fall$Lat, 
  Lon_i = bluepyagg_stn_fall$Lon, 
  t_i = bluepyagg_stn_fall$Year, 
  b_i = bluepyagg_stn_fall$Catch_g,
  a_i = rep(1, nrow(bluepyagg_stn_fall)), 
  working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

# Run model spring

season <- c("spring_500_noomeps12_noaniso")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}                         
                          

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
