# VAST attempt 2 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

bluepyagg_stn <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

#bluepyagg_stn <- bluepyagg_pred_stn

# filter to assessment years at Tony's suggestion

# code Vessel as AL=0, HB=1, NEAMAP=2

bluepyagg_stn_fall <- bluepyagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL",
         year > 1984) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
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
  mutate(AreaSwept_km2 =1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
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

# Model selection options, 
# Season_knots + suffix below
#                           FieldConfig default (all IID)
# _noaniso                  FieldConfig default (all IID) and use_anistropy = FALSE
# _noomeps2                 FieldConfig 0 for Omega2, Epsilon2
# _noomeps2_noaniso         FieldConfig 0 for Omega2, Epsilon2 and use_anistropy = FALSE
# _noomeps2_noeps1          FieldConfig 0 for Omega2, Epsilon2, Omega1
# _noomeps2_noeps1_noaniso  FieldConfig 0 for Omega2, Epsilon2, Omega1 and use_anistropy = FALSE
# _noomeps12                FieldConfig all 0
# _noomeps12_noaniso        FieldConfig all 0 and use_anistropy = FALSE

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

OverdispersionConfig	<- c("eta1"=1, "eta2"=1)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

strata.limits <- as.list(MABGBGOMSS)

settings = make_settings( n_x = 500, 
                          Region = "northwest_atlantic",
                          #strata.limits = list('All_areas' = 1:1e5), full area
                          strata.limits = strata.limits,
                          purpose = "index2", 
                          bias.correct = FALSE,
                          #use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          #FieldConfig = FieldConfig,
                          #RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig
                          )

 #Aniso=FALSE, #correct ln_H_input at bound
 #FieldConfig["Epsilon1"]=0, #correct L_epsilon1_z approaching 0  
 #FieldConfig["Epsilon2"]=0 #correct L_epsilon2_z approaching 0 
 #increase knots to address bounds for logkappa?
 # https://github.com/James-Thorson-NOAA/VAST/issues/300
 # or try finescale=FALSE
 # then Omegas hit bounds, had to turn then off too

# select dataset and set directory for output

season <- c("fall_500_wneamap_eta11")

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
  b_i = as_units(bluepyagg_stn_fall[,'Catch_g'], 'g'),
  a_i = rep(1, nrow(bluepyagg_stn_fall)),
  v_i = bluepyagg_stn_fall$Vessel,
  #Use_REML = TRUE,
  working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

# Run model spring

season <- c("spring_500_wneamap_eta11")

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
                 v_i = bluepyagg_stn_spring$Vessel,
                # Use_REML = TRUE,
                 working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 
