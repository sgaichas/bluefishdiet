# correct erroneous prey weight in station id NM20070901011 (Fall 2007)
# corrected info in fhdat/NEAMAP_Mean stomach weights_Bluefish PreyWQ2_CORRECTED STATION.csv

# update the sumbluepywt and meanbluepywt columns
# new values are 4.8404 and 0.186169231, respectively

# read in all datasets to check for this station number--initial dataset, one called all, and oe with OISST

# doesn't have NEAMAP but double check
bluepyagg_stn <- readRDS("~/Documents/0_Data/bluefishdiet/fhdat/bluepyagg_stn.rds")

bluepyagg_stn %>% filter(id == "NM20070901011") # returns tibble 0x18

# should have NEAMAP
bluepyagg_stn_all <- readRDS("~/Documents/0_Data/bluefishdiet/fhdat/bluepyagg_stn_all.rds")

bluepyagg_stn_all %>% filter(id == "NM20070901011") # has this station

bluepyagg_stn_all$sumbluepywt[bluepyagg_stn_all$id == "NM20070901011"] <- 4.8404
bluepyagg_stn_all$meanbluepywt[bluepyagg_stn_all$id == "NM20070901011"] <- 0.186169231

saveRDS(bluepyagg_stn_all, file = "~/Documents/0_Data/bluefishdiet/fhdat/bluepyagg_stn_all.rds")

# should have NEAMAP
bluepyagg_stn_all_OISST <- readRDS("~/Documents/0_Data/bluefishdiet/fhdat/bluepyagg_stn_all_OISST.rds")

bluepyagg_stn_all_OISST %>% filter(id == "NM20070901011") # has this station

bluepyagg_stn_all_OISST$sumbluepywt[bluepyagg_stn_all$id == "NM20070901011"] <- 4.8404
bluepyagg_stn_all_OISST$meanbluepywt[bluepyagg_stn_all$id == "NM20070901011"] <- 0.186169231

saveRDS(bluepyagg_stn_all_OISST, file = "~/Documents/0_Data/bluefishdiet/fhdat/bluepyagg_stn_all_OISST.rds")
