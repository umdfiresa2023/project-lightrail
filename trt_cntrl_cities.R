library("tidyverse")
library("readxl")

transit <- read_xlsx("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/December 2022 Raw Database_0.xlsx", 
                     sheet="Calendar_Year_UPT")

#treatment cities must open light rails from 2004 to 2013 or after
 
# --- Light Rails
lr<-transit %>%
  filter(Modes=="LR" | Modes=="SR" | Modes=='YR' | Modes=='CR') %>%
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

# --- Buses
buses<-transit %>%
  filter(Modes=="MB" | Modes=="CB" | Modes=='RB') %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  filter(!is.na(`UZA Name`)) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

bus_long<-pivot_longer(buses, !`UZA Name`, names_to = "Year", values_to = "BUS_UPT")

df3<-merge(df2, bus_long, all.x = TRUE)

df4<-df3 %>%
  mutate(Bus_share=BUS_UPT/all_UPT)

# --- Monorail / Automated Guideway 
mono<-transit %>%
  filter(Modes=="AG" | Modes=="MO" | Modes=='MG') %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  filter(!is.na(`UZA Name`)) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

mono_long<-pivot_longer(mono, !`UZA Name`, names_to = "Year", values_to = "Mono_UPT")

df5<-merge(df4, mono_long, all.x = TRUE)

df6<-df5 %>%
  mutate(Mono_share=Mono_UPT/all_UPT)

# --- HR (Heavy Rail)
hr<-transit %>%
  filter(Modes=="HR") %>%
  dplyr::select(-`5 digit NTD ID`, -`4 digit NTD ID`, -Agency, -`Reporter Type`,
                -UZA, -Modes, -TOS, -Active) %>%
  filter(!is.na(`UZA Name`)) %>%
  replace(is.na(.),0) %>%
  group_by(`UZA Name`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

hr_long<-pivot_longer(hr, !`UZA Name`, names_to = "Year", values_to = "Hr_UPT")

df7<-merge(df6, hr_long, all.x = TRUE)

df8<-df7 %>%
  mutate(Hr_share=Hr_UPT/all_UPT) %>%
  replace(is.na(.),0) %>%
  mutate(Year=as.numeric(Year))

#Make this spreadsheet for all types of transportation (buses, subways)
#add conditions that you deem important

trt_df<-df8 %>%
  filter((Year==2002 & LR_UPT==0) |
           (Year==2003 & LR_UPT==0) |
           (Year==2014 & LR_UPT>0) |
           (Year==2015 & LR_UPT>0) |
           (Year==2016 & LR_UPT>0)) %>%
  group_by(`UZA Name`) %>%
  mutate(count=n()) %>%
  filter(count==5) 

lr_cities<-unique(trt_df$`UZA Name`)
i<-1

lr_start<-df8 %>%
  filter(`UZA Name` %in% lr_cities) %>%
  filter(LR_share>0) %>%
  group_by(`UZA Name`) %>%
  summarize(start_year=min(Year), pt=mean(all_UPT)) %>%
  filter(start_year<2013 & start_year>=2004)

i<-14

lrdf<-df8 %>%
  filter(`UZA Name`== lr_start$`UZA Name`[i]) %>%
  filter(Year>= lr_start$start_year[i]-4) %>%
  filter(Year<= lr_start$start_year[i]+4)

transit2 <- transit %>%
  group_by(Modes) %>% 
  mutate(riders=ifelse(is.na(`2020`), 0, `2020`)) %>%
  summarize(Total_riders=(sum(riders)))
