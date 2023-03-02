library("tidyverse")
library("readxl")

transit <- read_xlsx("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/December 2022 Raw Database_0.xlsx", 
                     sheet="Calendar_Year_UPT")

#treatment cities must open light rails from 2004 to 2013 or after
 
# ---
lr<-transit %>%
  filter(Modes=="LR" | Modes=="SR") %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

lr_long<-pivot_longer(lr, !`UZA Name`, names_to = "Year", values_to = "LR_UPT")

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
# ---

# creating mode_UPT and mode_share for all other modes 
modes<-c("AG", "MO", "CR", "AR", "CB", "CC", "DR", "FB", "HR", "IP", "MB", "MG", "OR", "PB",  
           "RB", "TB", "TR", "VP", "YR")
  
for(mode in modes) {
  df3<-transit %>%
    filter(Modes==mode) %>%
    dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                  -UZA, -Modes, -TOS, -Active) %>%
    replace(is.na(.),0) %>%
    group_by(`UZA Name`) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  df3_long<-pivot_longer(df3, !`UZA Name`, names_to = "Year", values_to = paste(mode, "_UPT"))
  
  df4<-merge(all_long, df3_long, all.x = TRUE)
  
  new_col_name = paste(mode, "_share")
  col_name = paste(mode, "_UPT")
  
  df5<-df4 %>%
    mutate(new_col_name = col_name/all_UPT)
}

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

# To determine if ridership in a certain mode is generally consistent over the years, we can 
# use variance. 
# To calculate sample variance, place all values in a vector, then do var(vector_name).
# To calculate population variance, have a vector of all values. Size: n<-length(data). 
# Use this formula: var(data)*(n-1)/n
