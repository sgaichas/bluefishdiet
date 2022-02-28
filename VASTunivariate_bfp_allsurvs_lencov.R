# VAST attempt 2 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in UpdateVAST_Inputs.Rmd

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
  dplyr::select(Catch_g = meanbluepywt, #use bluepywt for individuals input in example
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanpisclen,
         npiscsp,
         #bottemp #this leaves out many stations for NEFSC
         ) %>%
  na.omit() %>%
  as.data.frame()

bluepyagg_stn_spring <- bluepyagg_stn %>%
  filter(season_ng == "SPRING",
         year > 1984) %>%
  mutate(AreaSwept_km2 =1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanbluepywt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanpisclen,
         npiscsp,
         #bottemp #this leaves out many stations for NEFSC
         ) %>%
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

# Model selection options, FieldConfig default (all IID)
# Season_knots + suffix below 
# _base         No vessel overdispersion or length/number covariates  (ensure same dataset)  
# _len          Predator mean length covariate
# _no           Number of predator species covariate
# _lenno        Predator mean length and number of predator species covariates
# _eta10        Overdispersion (vessel effect) in first linear predictor (prey encounter)
# _eta11        Overdispersion (vessel effect) in both linear predictors (prey wt)

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

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
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


# select dataset and set directory for output

#########################################################
# Run model fall

season <- c("fall_500_base")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}


fit <- fit_model(
  settings = settings, 
  Lat_i = bluepyagg_stn_fall$Lat, 
  Lon_i = bluepyagg_stn_fall$Lon, 
  t_i = bluepyagg_stn_fall$Year, 
  b_i = as_units(bluepyagg_stn_fall[,'Catch_g'], 'g'),
  a_i = rep(1, nrow(bluepyagg_stn_fall)),
  v_i = bluepyagg_stn_fall$Vessel,
  #Q_ik = as.matrix(bluepyagg_stn_fall[,c("npiscsp")]),
  #Use_REML = TRUE,
  working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

######################################################
# Run model spring

season <- c("spring_500_base")

working_dir <- here::here(sprintf("pyindex/allagg_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}                         
                          

fit = fit_model( settings = settings, 
                 Lat_i = bluepyagg_stn_spring[,'Lat'], 
                 Lon_i = bluepyagg_stn_spring[,'Lon'], 
                 t_i = bluepyagg_stn_spring[,'Year'], 
                 b_i = as_units(bluepyagg_stn_spring[,'Catch_g'], 'g'), 
                 a_i = rep(1, nrow(bluepyagg_stn_spring)),
                 v_i = bluepyagg_stn_spring$Vessel,
                # Q_ik = as.matrix(bluepyagg_stn_spring[,c("npiscsp")]),
                # Use_REML = TRUE,
                 working_dir = paste0(working_dir, "/"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 
