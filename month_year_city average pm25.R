library("tidyverse")

path<-"G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/PM25_daily/"
months<-dir(path)

monthly_avg_pm25<-c()

# find average pm25 for each city-month-year, append each df to the final df
for(m in 1:length(months)) {
  df<-read.csv(paste0(path, months[m]))
  
  df2<-df %>%
    mutate(year=substr(months[m], 1, 4)) %>%
    mutate(month=substr(months[m], 6, 7)) %>%
    group_by(city, year, month) %>%
    summarize(avg_pm25=mean(mean_pm25))
  
  monthly_avg_pm25<-rbind(monthly_avg_pm25, df2)
}

# remove duplicates
monthly_avg_pm25 <- monthly_avg_pm25[!duplicated(monthly_avg_pm25), ]
