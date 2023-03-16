# compare 2022 prey list to paper update preylist for indices

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
                                    "Stratum_13",
                                    "Stratum_14",
                                    "Stratum_15"),
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
                                    "albbfin",
                                    "albbfall",
                                    "allother"))

splitoutold <- read.csv("pyindex/2022/allagg_fall_500_lennosst_ALLsplit_biascorrect/Index.csv")
splitoutnew <- read.csv("pyindex/allagg_fall_500_lennosst_ALLsplit_biascorrect/Index.csv")

splitoutput2022 <- splitoutold %>%
  dplyr::left_join(stratlook) %>%
  dplyr::filter(Region %in% c("GOM", "GB", "MAB","MABGBstate", "bfin")) %>%
  dplyr::mutate(Type = ifelse(Region %in% c("GOM", "GB", "MAB"), "Ecoregion", "Bluefish"),
                Region = case_when(Region == "MABGBstate" ~ "StateWaters",
                                   Region == "bfin" ~ "SurveyBluefish",
                                   TRUE ~ Region))

splitoutput <- splitoutnew %>%
  dplyr::left_join(stratlook) %>%
  dplyr::filter(Region %in% c("GOM", "GB", "MAB","MABGBstate", "bfin")) %>%
  dplyr::mutate(Type = ifelse(Region %in% c("GOM", "GB", "MAB"), "Ecoregion", "Bluefish"),
                Region = case_when(Region == "MABGBstate" ~ "StateWaters",
                                   Region == "bfin" ~ "SurveyBluefish",
                                   TRUE ~ Region))

foragemax <- max(splitoutput$Estimate) #scale to new prey list

fig <- ggplot(splitoutput, aes(x=Time, y=Estimate, colour=Region)) +
  geom_errorbar(aes(ymin=Estimate+Std..Error.for.Estimate, ymax=Estimate-Std..Error.for.Estimate))+
  geom_point()+
  geom_line()+
  facet_wrap(~fct_relevel(Type, "Ecoregion", "Bluefish"), scales = "free_y") +
  scale_y_continuous(labels=function(x)round(x/foragemax, digits = 2))+
  ylab("Relative forage biomass scaled to maximum") 



EPU <- ggplot(splitoutput %>% dplyr::filter(Region %in% c("GOM", "GB", "MAB")), 
              aes(x=Time, y=Estimate, colour=Region)) +
  geom_errorbar(aes(ymin=Estimate+Std..Error.for.Estimate, ymax=Estimate-Std..Error.for.Estimate))+
  geom_point()+
  geom_line()+
  geom_line(data=splitoutput2022 %>% dplyr::filter(Region %in% c("GOM", "GB", "MAB")),
            aes(x=Time, y=Estimate, colour=Region), linetype = "dashed") +
  facet_wrap(~fct_relevel(Region, "GOM", "GB", "MAB"), scales = "free_y", ncol = 1) +
  scale_y_continuous(labels=function(x)round(x/foragemax, digits = 2))+
  ylab("Relative forage biomass scaled to maximum") 

BF <- ggplot(splitoutput %>% dplyr::filter(Region %in% c("StateWaters", "SurveyBluefish")), 
             aes(x=Time, y=Estimate, colour=Region)) +
  geom_errorbar(aes(ymin=Estimate+Std..Error.for.Estimate, ymax=Estimate-Std..Error.for.Estimate))+
  geom_point()+
  geom_line()+
  geom_line(data=splitoutput2022 %>% dplyr::filter(Region %in% c("StateWaters", "SurveyBluefish")),
            aes(x=Time, y=Estimate, colour=Region), linetype = "dashed") +
  facet_wrap(~fct_relevel(Region, "StateWaters", "SurveyBluefish"), scales = "free_y", ncol = 1) +
  scale_y_continuous(labels=function(x)round(x/foragemax, digits = 2))+
  ylab("Relative forage biomass scaled to maximum")
