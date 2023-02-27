library("tidyverse")
library("readxl")

Transit <- read_xlsx("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/December 2022 Raw Database_0.xlsx", 
                     sheet="Calendar_Year_UPT")

#treatment cities must open light rails from 2004 or after
LR<-Transit %>%
  filter(Modes=="LR" | Modes=="SR") %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

LR_long<-pivot_longer(LR, !`UZA Name`, names_to = "Year", values_to = "LR_UPT")
