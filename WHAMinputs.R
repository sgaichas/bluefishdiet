# create csv for WHAM input
# long input best?
# going wide...

library(dplyr)
library(ggplot2)
library(tidyr)

WHAMinputs <- function(infile, outfile) {
  
  splitoutput <- read.csv(infile)
  
  # warning, hardcoded. obviously
  stratlook <- data.frame(Stratum = c("Stratum_1",
                                      "Stratum_2",
                                      "Stratum_3",
                                      "Stratum_4",
                                      "Stratum_5",
                                      "Stratum_6",
                                      "Stratum_7",
                                      "Stratum_8",
                                      "Stratum_9",
                                      "Stratum_10",
                                      "Stratum_11",
                                      "Stratum_12",
                                      "Stratum_13"),
                          Region  = c("AllEPU", 
                                      "MABGB", 
                                      "MABGBstate", 
                                      "MABGBfed", 
                                      "MAB",
                                      "GB",
                                      "GOM",
                                      "bfall",
                                      "bfin",
                                      "bfoff",
                                      "MABGBalbinshore",
                                      "MABGBothoffshore",
                                      "allother"))
  
  forageindex <- splitoutput %>%
    left_join(stratlook) %>%
    dplyr::select(Time, Region, Estimate, SE=Std..Error.for.Estimate) %>%
    tidyr::pivot_longer(c(Estimate, SE), names_to = "Var") %>%
    dplyr::group_by(Var) %>%
    tidyr::pivot_wider(names_from = Region, values_from = value) %>%
    dplyr::mutate(BigOld = bfin,
                  BigNew = bfall,
                  AlbOld = MABGBalbinshore + BigOld, #summed SE a slight overestimate
                  AlbNew = MABGBalbinshore + BigNew, #summed SE a slight overestimate
                  StateWaters = MABGBstate,
                  FedWaters =   MABGBfed) %>%
    dplyr::select(Time, BigOld, BigNew, AlbOld, AlbNew, StateWaters, FedWaters) %>%
    tidyr::pivot_longer(!c(Time, Var), names_to = "Region", values_to = "value") %>%
    tidyr::pivot_wider(names_from = "Var", values_from = "value") %>%
    tidyr::pivot_wider(names_from = "Region", values_from = c("Estimate", "SE"),
                       names_glue = "{Region}_{.value}", names_vary = "slowest")
  
  write_csv(forageindex, outfile)
  
} 


# make data files
WHAMinputs(infile = "pyindex/allagg_fall_500_lennosst_ALLsplit_biascorrect/Index.csv",
           outfile = "toWHAM/fallforageindex.csv")

WHAMinputs(infile = "pyindex/allagg_spring_500_lennosst_ALLsplit_biascorrect/Index.csv",
           outfile = "toWHAM/springforageindex.csv")

WHAMinputs(infile = "pyindex/allagg_annual_500_lennosst_ALLsplit_biascorrect/Index.csv",
           outfile = "toWHAM/annualforageindex.csv")



# bias corrected fall results
#splitoutput <- read.csv("pyindex/allagg_fall_500_lennosst_ALLsplit_biascorrect/Index.csv")

# # code for visualizing summed SE approximation vs calculated SE
# in2off <- splitoutput %>%
#   left_join(stratlook) %>%
#   dplyr::select(Time, Region, Estimate, Std..Error.for.Estimate) %>%
#   tidyr::pivot_longer(c(Estimate, Std..Error.for.Estimate), names_to = "Var") %>%
#   dplyr::group_by(Var) %>%
#   tidyr::pivot_wider(names_from = Region, values_from = value) %>%
#   dplyr::mutate(AlbInshore = MABGBalbinshore,
#                 NewOffshore = bfoff,
#                 BigOld = bfin,
#                 BigNew = bfall,
#                 CompBigNew = bfin + bfoff,
#                 AlbOld = AlbInshore + BigOld,
#                 AlbNew = AlbInshore + BigNew,
#                 StateWaters = MABGBstate,
#                 FedWaters =   MABGBfed) %>%
#   dplyr::select(Time, AlbInshore, NewOffshore, BigOld, BigNew, CompBigNew, AlbOld, AlbNew, StateWaters, FedWaters) %>%
#   tidyr::pivot_longer(!c(Time, Var), names_to = "Region", values_to = "value") %>%
#   tidyr::pivot_wider(names_from = "Var", values_from = "value")
# 
# ggplot(in2off, aes(x=Time, y=Estimate, colour = Region)) +
#   geom_errorbar(aes(ymin=Estimate+Std..Error.for.Estimate, ymax=Estimate-Std..Error.for.Estimate))+
#   geom_point()+
#   geom_line()
# 
# compare <- in2off %>%
#   filter(Region %in% c("CompBigNew", "BigNew")) 
# 
# ggplot(compare, aes(x=Time, y=Estimate, colour = Region)) +
#   geom_errorbar(aes(ymin=Estimate+Std..Error.for.Estimate, ymax=Estimate-Std..Error.for.Estimate))+
#   geom_point()+
#   geom_line()

