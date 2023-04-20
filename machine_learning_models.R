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


# Upload and clean data
df2<-updated2 %>%
  filter(lr_month<=36 & lr_month>= -36)

df3<-df2 %>%
  filter(lr_op < 1) %>%
  dplyr::select(-lr_op, -lr_month)

sapply(lapply(df2, unique), length)

# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)

train<-shuffled %>%
  dplyr::slice(1:100)

test<-shuffled %>%
  dplyr::slice(101:145)

# random forest model

model4 <- ranger(new_monthly_avg ~ ., data=train, importance='impurity')
v1 <- vip(model4)
v1

#rf model evaluation
predict_test<-predict(model4, test)

rmse_test_rf<-sqrt(mean(test$new_monthly_avg-predict_test$predictions)^2)

rmse_test_rf

#create data for the graph
openlr <- updated %>%
  filter(lr_op>0) %>%
  mutate(city=as.factor(city))%>%
  dplyr::select(-temp, -date, -meanpm25) %>%
  dplyr::select(-lr_op, -lr_month)

# rf model with all data
predict_test<-predict(model4, openlr)

rf_pred<-predict_test$predictions

rf_df<-cbind(openlr, rf_pred)

# ------------------------------------------------------------------------------

# XGBoost model
# install.packages("xgboost")

# Split data in to test and train group
set.seed(112)

shuffled<-df3 %>% sample_frac(size=1, replace=FALSE)
all_data <- df2 %>%
  dplyr::select(-lr_op)
all_data2 <- data.matrix(all_data)

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

#create data for the graph
openlr <- updated %>%
  filter(lr_op>0) %>%
  mutate(city=as.factor(city))%>%
  dplyr::select(-temp, -date, -meanpm25) %>%
  dplyr::select(-lr_op, -lr_month)

openlr_matrix <- data.matrix(openlr)

all_x = data.matrix(openlr_matrix[, -4])
all_y = openlr_matrix[, 4]

xgb_all = xgb.DMatrix(data = all_x, label = all_y)

# rf model with all data
predict_all_xgb<-predict(xgb_model, xgb_all)

xgb_df<-cbind(rf_df, predict_all_xgb)

# ------------------------------------------------------------------------------

openlr <- updated %>%
  filter(lr_op>0)

lr_removed <- df2 %>%
  dplyr::select(-lr_op)

# rf model with all data
model5 <- ranger(new_monthly_avg ~ ., data=lr_removed, importance='impurity')
predict_test<-predict(model4, lr_removed)

# xgb model with all data
matrix <- data.matrix(lr_removed)

my<-df2 %>%
  dplyr::select(lr_month, month, year, city)

pred_df<-merge(xgb_df, my, by=c("month","year", "city"), all.x=TRUE)

p<-ggplot() + 
  geom_line(data = df2, aes(x = lr_month, y = new_monthly_avg, color = "Actual Data"), size=1) + facet_wrap(~ city) +
  geom_line(data=pred_df, aes(x = lr_month, y = rf_pred, color = "RF Counterfactual"), size=1) + facet_wrap(~ city) +
  geom_line(data=pred_df, aes(x = lr_month, y = predict_all_xgb, color = "XGB Counterfactual"), size=1) + facet_wrap(~ city) + 
  #geom_smooth(data = pred_df, aes(x = lr_month, y = new_monthly_avg), color = "black", se = FALSE) + 
  #geom_smooth(data = pred_df, aes(x = lr_month, y = rf_pred), color = "red", se = FALSE) +
  #geom_smooth(data = pred_df, aes(x = lr_month, y = predict_all_xgb), color = "blue", se = FALSE) +
  xlab("Months since light rail opening") + ylab("Mean PM2.5 (μg/m3)") + geom_vline(xintercept=0, linetype="dashed")+theme_bw()

ggsave(p, filename="G:/Shared drives/2023 FIRE-SA PRM/Spring Research/Light Rails/OUTPUT/rf_xgb_graph.png", dpi=500, width=8, height=5, unit="in")
