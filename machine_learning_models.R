library("tidyverse")

updated <- read.csv("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/merged_pm25.csv")

# adjust data for machine learning models
updated<-updated %>%
  mutate(city=as.factor(city))

updated<-updated[,-c(5,6)]

# Machine Learning Models

# rpart model

# Upload and clean data
df2<-updated %>%
  filter(lr_month<=36 & lr_month>= -36)

summary(updated)

df3<-df2 %>%
  filter(lr_op < 1) %>%
  dplyr::select(-lr_op, -lr_month)

glimpse(df2)

sapply(lapply(df2, unique), length)

# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>%sample_frac(size=1, replace=FALSE)

train<-shuffled %>%
  slice(1:100)

test<-shuffled %>%
  slice(101:145)

# Run linear model and calculate test RMSE
model1<-lm(new_monthly_avg ~ ., data=train)

summary(model1)

predict_test<-predict(model1, test, type="response")

rmse_test_lm1<-sqrt(mean(test$new_monthly_average-predict_test)^2)

rmse_test_lm1

# Run linear model and calculate test RMSE 
library("leaps")

Best_Subset <-
  regsubsets(meanpm25~.,
             data = train,
             nbest = 1,      
             nvmax = 5,    # limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "seqrep")

summary_best_subset <- summary(Best_Subset)
summary_best_subset$which[which.max(summary_best_subset$adjr2),]

model2<-lm(meanpm25 ~ temp + mt2 + vp2 + city, data=train)

summary(model2)

predict_test<-predict(model2, test, type="response")

rmse_test_lm2<-sqrt(mean(test$meanpm25-predict_test)^2)

rmse_test_lm2

# Run rpart package
library("rpart")
model3<-rpart(meanpm25 ~., train)

summary(model3)

library("rpart.plot")

rpart.plot(model3,fallen.leaves = T)

library("vip")

v1<-vip(model3)
v1

predict_test<-predict(model3, test)

rmse_test_rpart<-sqrt(mean(test$meanpm25-predict_test)^2)

rmse_test_rpart

# ------------------------------------------------------------------------------

# random forest model

library("ranger")
library("vip")

model4 <- ranger(new_monthly_avg ~ ., data=train, importance='impurity')
v1 <- vip(model4)
v1

predict_test<-predict(model4, test)

rmse_test_rf<-sqrt(mean(test$new_monthly_avg-predict_test$predictions)^2)

rmse_test_rf

