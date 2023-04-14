library("tidyverse")
library("dplyr")
library("leaps")
library("rpart")
library("rpart.plot")
library("vip")
library("ranger")
library("xgboost")

# install.packages("caret")

updated <- read.csv("G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/DATA/merged_pm25.csv")

# adjust data for machine learning models
updated2<-updated %>%
  mutate(city=as.factor(city))%>%
  dplyr::select(-temp, -date, -meanpm25)

# Machine Learning Models:

# rpart model

# Upload and clean data
df2<-updated2 %>%
  filter(lr_month<=36 & lr_month>= -36)

summary(updated)

df3<-df2 %>%
  filter(lr_op < 1) %>%
  dplyr::select(-lr_op, -lr_month)

glimpse(df2)

sapply(lapply(df2, unique), length)

# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)

train<-shuffled %>%
  slice(1:100)

test<-shuffled %>%
  slice(101:145)

# Run linear model and calculate test RMSE
model1<-lm(new_monthly_avg ~ ., data=train)

summary(model1)

predict_test<-predict(model1, test, type="response")

rmse_test_lm1<-sqrt(mean(test$new_monthly_avg-predict_test)^2)

rmse_test_lm1

# Run linear model and calculate test RMSE 
Best_Subset <-
  regsubsets(new_monthly_avg~.,
             data = train,
             nbest = 1,      
             nvmax = 5,    # limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "seqrep")

summary_best_subset <- summary(Best_Subset)
summary_best_subset$which[which.max(summary_best_subset$adjr2),]

model2<-lm(new_monthly_avg ~ Mean_Temperature + mt2 + vp2 + city, data=train)

summary(model2)

predict_test<-predict(model2, test, type="response")

rmse_test_lm2<-sqrt(mean(test$new_monthly_avg-predict_test)^2)

rmse_test_lm2

# Run rpart package
model3<-rpart(new_monthly_avg ~., train)

summary(model3)

rpart.plot(model3,fallen.leaves = T)

v1<-vip(model3)
v1

predict_test<-predict(model3, test)

rmse_test_rpart<-sqrt(mean(test$new_monthly_avg-predict_test)^2)

rmse_test_rpart

# ------------------------------------------------------------------------------

# random forest model

model4 <- ranger(new_monthly_avg ~ ., data=train, importance='impurity')
v1 <- vip(model4)
v1

predict_test<-predict(model4, test)

rmse_test_rf<-sqrt(mean(test$new_monthly_avg-predict_test$predictions)^2)

rmse_test_rf

# ------------------------------------------------------------------------------

# XGBoost model
# install.packages("xgboost")

# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)

train<-shuffled %>%
  dplyr::slice(1:100)

test<-shuffled %>%
  dplyr::slice(101:145)

#define predictor and response variables in training set
train_x = data.matrix(train[, -4])
train_y = train[,4]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -4])
test_y = test[, 4]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model
xgb_model = xgboost(data = xgb_train, max.depth = 3, nrounds = 14, verbose = 0)

predict_xgb<-predict(xgb_model, xgb_test)

rmse_test_xgb<-sqrt(mean(test_y-predict_xgb)^2)

rmse_test_xgb

# ------------------------------------------------------------------------------

# Comapring models

openlr <- updated %>%
  filter(lr_op>0)

predict_test<-predict(model4, df2)

df3 <- cbind(df2, predict_test$predictions)

ggplot(data = df3) + geom_point(aes(x = lr_month, y = new_monthly_avg)) +
  geom_point(aes(x = lr_month, y = predict_test$predictions, color = "predicted")) + facet_wrap(~ city)+ 
  geom_smooth(aes(x = lr_month, y = new_monthly_avg), color = "black", se = FALSE) + 
  geom_smooth(aes(x = lr_month, y = predict_test$predictions), color = "red", se = FALSE) +
  xlab("month") + ylab("mean of PM2.5") + geom_vline(xintercept=0, linetype="dashed")+theme_bw()
