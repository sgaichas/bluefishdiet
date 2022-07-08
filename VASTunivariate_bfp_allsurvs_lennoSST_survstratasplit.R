# VAST attempt 2 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in SSTmethods.Rmd

bluepyagg_stn <- readRDS(here::here("fhdat/bluepyagg_stn_all_OISST.rds"))

# make SST column that uses surftemp unless missing or 0
# there are 3 surftemp 0 values in the dataset, all with oisst > 15
bluepyagg_stn <- bluepyagg_stn %>%
  dplyr::mutate(sstfill = ifelse((is.na(surftemp)|surftemp==0), oisst, surftemp))

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
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         oisst,
         sstfill
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
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         oisst,
         sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()


# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302

#current bluefish assessment strata are all Bigelow inshore strata MAB-GB
bfinshore <- c(3020, 3050, 3080, 3110, 3140, 3170, 3200, 3230, 
               3260, 3290, 3320, 3350, 3380, 3410, 3440, 3450, 3460)

bfin <-  northwest_atlantic_grid %>%
  filter(stratum_number %in% bfinshore) %>%
  select(stratum_number) %>% 
  distinct()

# from Tony's 8 March presentation, minus the inshore in CCBay
bfoffshore <- c(1010, 1730, 1690, 1650, 1050, 1060, 1090, 1100, 1250, 1200, 1190, 1610)

bfoff <- northwest_atlantic_grid %>%
  filter(stratum_number %in% bfoffshore) %>%
  select(stratum_number) %>% 
  distinct()

bfall <- northwest_atlantic_grid %>% 
  filter(stratum_number %in% c(bfinshore, bfoffshore)) %>% 
  select(stratum_number) %>% 
  distinct()


#from mskeyrun vignette, EPU based on survey strata, replace built in VAST EPU
#https://noaa-edab.github.io/ms-keyrun/articles/GBSurveySet.html

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

AllEPU <- northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% c(MAB, GB, GOM, SS)) %>% 
  dplyr::select(stratum_number) %>% 
  distinct()

MABGB <- northwest_atlantic_grid %>%
  dplyr::filter(stratum_number %in% c(MAB, GB)) %>% 
  dplyr::select(stratum_number) %>% 
  distinct()

albinshore <- MABGB %>%
  dplyr::filter(stratum_number>2999 & stratum_number<3999) %>% #inshore
  dplyr::filter(!(stratum_number %in% bfinshore)) %>% 
  distinct() 

othoffshore <- MABGB %>%
  dplyr::filter(stratum_number<2000) %>%
  dplyr::filter(!(stratum_number %in% bfoffshore)) %>% 
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

strata.limits <- as.list(c("AllEPU" = AllEPU, 
                         "MABGB" = MABGB, 
                         "albinshore" = albinshore,
                         "bfinshore" = bfin,
                         "bfoffshore" = bfoff,                         
                         "bfall" = bfall,
                         "othoffshore" = othoffshore
                         ))

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

season <- c("fall_500_lennosst_split_nobiascorrect")

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
  Q_ik = as.matrix(bluepyagg_stn_fall[,c("npiscsp", 
                                         "meanpisclen", 
                                         "sstfill"
                                         )]),
  #Use_REML = TRUE,
  working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))



######################################################
# Run model spring

season <- c("spring_500_lennosst_split_nobiascorrect")

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
                 Q_ik = as.matrix(bluepyagg_stn_spring[,c("npiscsp", 
                                                          "meanpisclen", 
                                                          "sstfill"
                                                          )]),
                # Use_REML = TRUE,
                 working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 


