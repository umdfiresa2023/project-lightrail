library("tidyverse")
library("readxl")

transit <- read_xlsx("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/December 2022 Raw Database_0.xlsx", 
                     sheet="Calendar_Year_UPT")

#treatment cities must open light rails from 2004 to 2013 or after

lr<-transit %>%
  filter(Modes=="LR" | Modes=="SR") %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

lr_long<-pivot_longer(LR, !`UZA Name`, names_to = "Year", values_to = "LR_UPT")

all<-transit %>%
  filter(!is.na(`UZA Name`)) %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

all_long<-pivot_longer(all, !`UZA Name`, names_to = "Year", values_to = "all_UPT")

df<-merge(all_long, lr_long, all.x = TRUE)

df2<-df %>%
  mutate(LR_share=LR_UPT/all_UPT)

#Make this spreadsheet for all types of transportation (buses, subways)
#add conditions that you deem important

trt_df<-df2 %>%
  filter((Year==2002 & LR_UPT==0) | 
           (Year==2003 & LR_UPT==0) |
           (Year==2014 & LR_UPT>0) |
           (Year==2015 & LR_UPT>0) |
           (Year==2016 & LR_UPT>0)) %>%
  group_by(`UZA Name`) %>%
  mutate(count=n()) %>%
  filter(count==5)
