# -- Intro / Header ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(pastecs)
library(reshape2)
library(forecast)
library(mgcv)
library(rpart)
library(rpart.plot)
library(neuralnet)
# library(Metrics)

options(scipen = 100, digits = 6)


# --------- Load CSV file. MySQL export is in one file, with a binary flag ---------------
# 6493 test.csv
# 10886 train.csv
# 17379 total

bikeall.df <- read.csv("csv-inputs/kaggle_data_plus.csv", na.strings = "\\N", header = TRUE)

# Convert a few things to factors
# bikeall.df[,'hour']<-factor(bikeall.df[,'hour'])
bikeall.df[,'dayofweek']<-factor(bikeall.df[,'dayofweek'])
bikeall.df[,'season']<-factor(bikeall.df[,'season'])
bikeall.df[,'holiday']<-factor(bikeall.df[,'holiday'])
bikeall.df[,'workingday']<-factor(bikeall.df[,'workingday'])
bikeall.df[,'weather']<-factor(bikeall.df[,'weather'])
bikeall.df[,'house']<-factor(bikeall.df[,'house'])
bikeall.df[,'senate']<-factor(bikeall.df[,'senate'])
bikeall.df[,'capitals']<-factor(bikeall.df[,'capitals'])
bikeall.df[,'nationals']<-factor(bikeall.df[,'nationals'])
bikeall.df[,'united']<-factor(bikeall.df[,'united'])
bikeall.df[,'wizards']<-factor(bikeall.df[,'wizards'])
bikeall.df[,'sporting_event']<-factor(bikeall.df[,'sporting_event'])
bikeall.df[,'cua_session']<-factor(bikeall.df[,'cua_session'])
bikeall.df[,'au_session']<-factor(bikeall.df[,'au_session'])
bikeall.df[,'howard_session']<-factor(bikeall.df[,'howard_session'])
bikeall.df[,'session_any']<-factor(bikeall.df[,'session_any'])

# -- Scaling ---------------
# make a copy with columns we want to scale
columns.to.scale <- c("hour", "dayofweek", "season", "weather",
                      "temp", "temp_squared", "atemp", "humidity", "windspeed")
scaled_all.df <- subset(bikeall.df, select = columns.to.scale)

# Convert some of those columns back to numerics
scaled_all.df[,'dayofweek'] <- as.numeric(scaled_all.df[,'dayofweek'])
scaled_all.df[,'season'] <- as.numeric(scaled_all.df[,'season'])
scaled_all.df[,'weather'] <- as.numeric(scaled_all.df[,'weather'])

# Scale the numeric variables.
maxs <- apply(scaled_all.df, 2, max)
mins <- apply(scaled_all.df, 2, min)
scaled_all.df <- as.data.frame(scale(scaled_all.df, center = mins, scale = maxs - mins))

# Prepend 'scaled_' to scaled variables
colnames(scaled_all.df) <- paste("scaled", colnames(scaled_all.df), sep = "_")

# ... and recombine with main dataframe
bikeall.df <- data.frame(bikeall.df, scaled_all.df)
rm(scaled_all.df)


# Dataframe to build predictions for contest submission
biketest.df <- subset(bikeall.df, train == 0)

# All the vars for plotting. Using train == 1 because we need `count`
bikeplot.df <- subset(bikeall.df, train == 1)

# Shave off the highest outliers in count
bikeplot.df$count_shaved <- bikeplot.df$count
bikeplot.df$count_shaved[bikeplot.df$count_shaved > 
                            quantile(bikeplot.df$count_shaved, c(.90))] <- quantile(bikeplot.df$count_shaved, c(.90))

# Keep only variables we would use for predictions.
# Skipping stuff like casual/registered counts, individual sporting events/calendars.
# Put these in a data frames used to build models.
keeps <- c("count", "count_shaved", "hour", "dayofweek", "season", "holiday", "workingday", 
           "weather", "temp", "temp_squared", "atemp", "humidity", 
           "windspeed", "house", "senate", "sporting_event", "session_any",
           "scaled_hour", "scaled_dayofweek", "scaled_season", "scaled_weather", 
           "scaled_temp", "scaled_temp_squared", "scaled_atemp", "scaled_humidity", "scaled_windspeed")
biketrain.df <- bikeplot.df[keeps]

# Take out weather = 4. There are only 3 of these observations, which wreaks havoc with modeling.
# Many times, I get "Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor weather has new levels 4"
# biketrain.df$weather <- as.character(biketrain.df$weather)
# biketrain.df$weather[biketrain.df$weather =="4"] <- "3"
# biketrain.df$weather <- as.factor(biketrain.df$weather)



# -------------- Data shape & summary ----------------------------
dim(bikeall.df)
head(bikeall.df)

stat.desc(bikeall.df)

summary(bikeall.df)
median(bikeall.df$count)

str(bikeall.df)
describe(bikeall.df)
describe(bikeall.df$count)

describe(as.factor(bikeall.df$temp))
describe(as.factor(bikeall.df$atemp))
describe(as.factor(bikeall.df$humidity))

glimpse(bikeall.df)


# ++ Models ----------------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# randomly generate training and validation sets
set.seed(7)  # set seed for reproducing the partition

ss <- sample(1:2, size = nrow(biketrain.df), replace = TRUE, prob = c(0.6, 0.4))
# training.df = biketrain.df[ss==1,]
training.df = biketrain.df
validation.df = biketrain.df[ss==2,]



# --- "Multiple linear regression". Adapted from textbook. My best-guess 10 variables. ----------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE 1.31020 w/out humidity. ME 1.26723 RMSE 141.212
# RMSLE 1.28159 with humidity. ME 0.516658 RMSE 144.158
# LOTS of negative predictions to remove.

vars_to_use <- c('count', 'hour', 'dayofweek', 'season', 'workingday', 'humidity', # weather, 
                 'temp', 'windspeed', 'house', 'senate', 'session_any')

mlr_train.df <- subset(training.df, select = vars_to_use)
mlr_valid.df <- subset(validation.df, select = vars_to_use)

bike.lm <- lm(count ~ ., data = mlr_train.df, na.action = na.exclude)
# training: ME -0.0000000000135085 RMSE 137.355
pred_t <- predict(bike.lm, na.action = na.pass)
accuracy(pred_t, mlr_train.df$count)

# validation ME 0.93598 RMSE 137.619
pred_v <- predict(bike.lm, newdata = validation.df, na.action = na.pass)
accuracy(pred_v, validation.df$count)

#  use options() to ensure numbers are not displayed in scientific notation.
# options(scipen = 999)
summary(bike.lm)



# -- GAM regression. Single variable: temp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE  MAE     MPE     MAPE
# Test set 0.753854 166.9 127.19 -562.358 593.125
bike.gam <- gam(count ~ s(temp), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)
results <- data.frame(prediction = gam.train.pred, actual = training.df$count)
accuracy(results$prediction, results$actual)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)
results <- data.frame(prediction = gam.valid.pred, actual = validation.df$count)
accuracy(results$prediction, results$actual)

ggplot(bikeplot.df, aes(temp, count)) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))


# -- GAM regression. Single variable: atemp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE    MAE     MPE     MAPE
# Test set 0.761806 167.791 127.629 -570.92 601.669
bike.gam <- gam(count ~ s(atemp), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)
results <- data.frame(prediction = gam.train.pred, actual = training.df$count)
accuracy(results$prediction, results$actual)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)
results <- data.frame(prediction = gam.valid.pred, actual = validation.df$count)
accuracy(results$prediction, results$actual)



# -- GAM regression. Single variable: humidity -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE    MAE     MPE     MAPE
# Test set 0.477493 172.521 132.255 -650.922 681.644
bike.gam <- gam(count ~ s(humidity), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)
results <- data.frame(prediction = gam.train.pred, actual = training.df$count)
accuracy(results$prediction, results$actual)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)
results <- data.frame(prediction = gam.valid.pred, actual = validation.df$count)
accuracy(results$prediction, results$actual)


# require(xts)
# 
# time_index <- seq(from = as.POSIXct("2011-01-01 00:00"),
#                   to = as.POSIXct("2012-12-31 23:00"), by = "hour")
# set.seed(1)
# value <- rnorm(n = length(time_index))
# eventdata <- xts(value, order.by = time_index)
# ets(eventdata)
##

#  generate the naive and seasonal naive forecasts
# naive.pred <- naive(train.ts, h = nValid)
# snaive.pred <- snaive(train.ts, h = nValid)



# -- LM regression. Single variable: temp ---------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE    MAE     MPE     MAPE
# Test set 0.761806 167.791 127.629 -570.92 601.669
temp.lm <- lm(count ~ temp, data = training.df)

# predict training data and check accuracy
temp.lm.train.pred <- predict(temp.lm, na.action = na.pass)
results <- data.frame(prediction = temp.lm.train.pred, actual = training.df$count)
accuracy(results$prediction, results$actual)

# predict validation data and check accuracy
temp.lm.valid.pred <- predict(temp.lm, newdata = validation.df, na.action = na.pass)
results <- data.frame(prediction = temp.lm.valid.pred, actual = validation.df$count)
accuracy(results$prediction, results$actual)




# -- LM regression. Single variable: humidity ---------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE    MAE     MPE     MAPE
# Test set 0.642001 173.744 133.348 -666.854 697.702
humidity.lm <- lm(count ~ humidity, data = training.df)

# predict training data and check accuracy
humidity.lm.train.pred <- predict(humidity.lm, na.action = na.pass)
results <- data.frame(prediction = humidity.lm.train.pred, actual = training.df$count)
accuracy(results$prediction, results$actual)

# predict validation data and check accuracy
humidity.lm.valid.pred <- predict(humidity.lm, newdata = validation.df, na.action = na.pass)
results <- data.frame(prediction = humidity.lm.valid.pred, actual = validation.df$count)
accuracy(results$prediction, results$actual)







# -- linear regression with stepwise variable selection ----------------------------
# Throw everything at it.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE 1.64349. Bummer. ME 29.2465 RMSE 210.187

#### Table 6.6
# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both"
bike.lm <- lm(count ~ ., data = training.df, na.action = na.exclude)
bike.step.lm <- step(bike.lm, direction = "both")
summary(bike.step.lm)

bike.step.lm.pred <- predict(bike.step.lm, training.df)
# validation...  ME 1.607 RMSE 136.839
accuracy(bike.step.lm.pred, validation.df$count) 

residuals <- bike.step.lm.pred - validation.df$count
hist(residuals, breaks = 50, xlab = "residual (predicted - actual)")







# Regression tree  --------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# courtesy of tutorial: http://uc-r.github.io/regression_trees

# performing regression trees
m1 <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova"
)

rpart.plot(m1)
plotcp(m1) # Maximum size/depth of tree = 16, or so it seems.

m2 <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(m2) # Maximum size/depth looks way higher here.
abline(v = 16, lty = "dashed")
abline(v = 65, lty = "dashed")

# perform a grid search
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(55, 65, 1)
)

# iterate through each minsplit and maxdepth combination.
# save each model into its own list item.
models <- list()

for (i in 1:nrow(hyper_grid)) {
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = count ~ .,
    data = training.df,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


# -- Create Optimal tree ------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE of submitted data: 1.02692. ME: -2.11708 RMSE: 92.3636
optimal_tree <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova",
  control = list(minsplit = 19, maxdepth = 30, cp = 0.01)
)

rt.optimal.pred <- predict(optimal_tree, newdata = validation.df)
accuracy(rt.optimal.pred, validation.df$count)

rpart.plot(optimal_tree)


# -- Predict from competition data. Remove negatives and write to CSV ------------------------------------
# RMSLE: 1.02692
## Predictions with test/competition data.
rt.optimal.pred <- predict(optimal_tree, newdata = biketest.df, na.action = na.pass)

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(data.frame(datetime = biketest.df$datetime, count = rt.optimal.pred),
          file = "output/regression_tree_optimal.csv", row.names=FALSE)





# -- Bagging ???? --------------------------------------
# http://uc-r.github.io/regression_trees#bag
# See Chapter 13 for further details on bagging. ---

# -- random forest???
# -- boosted trees???





# -- Neural Network ------------------------------------
# https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/
# more complex: https://datascienceplus.com/fitting-neural-network-in-r/

# Take just the binary and scaled numeric predictors
vars_to_use <- c("count", "scaled_hour", "scaled_dayofweek", "scaled_season", "scaled_weather", 
                 "house", "senate", "scaled_temp", "scaled_humidity", "scaled_windspeed",
                 "session_any")
nn_data.df <- subset(biketrain.df, select = vars_to_use)
nn_test.df <- subset(biketest.df, select = vars_to_use)

# Change some factors to numeric
nn_data.df[,'house'] <- as.numeric(nn_data.df[,'house'])
nn_data.df[,'senate'] <- as.numeric(nn_data.df[,'senate'])
nn_data.df[,'session_any'] <- as.numeric(nn_data.df[,'session_any'])
# and for test
nn_test.df[,'house'] <- as.numeric(nn_test.df[,'house'])
nn_test.df[,'senate'] <- as.numeric(nn_test.df[,'senate'])
nn_test.df[,'session_any'] <- as.numeric(nn_test.df[,'session_any'])



# Split out train and valid data
scaled_train.df <- nn_data.df[ss==1,]
scaled_valid.df <- nn_data.df[ss==2,]


# nn <- neuralnet(count ~ hour + dayofweek + temp_squared + humidity + windspeed,
#                   +                 data = scaled_train.df, hidden = c(4, 2), rep = 3,
#                   +                 linear.output = TRUE)
# Error in if (ncol.matrix < rep) { : argument is of length zero

# Error in if (ncol.matrix < rep) { : argument is of length zero
# hidden = c(4, 2), rep = 4,

# Warning message: Algorithm did not converge in 3 of 3 repetition(s) within the stepmax. 
# nn <- neuralnet(count ~ hour + dayofweek + temp_squared + humidity + windspeed,
#                 data = scaled_train.df, hidden = c(4, 2), rep = 3,
#                 linear.output = TRUE)

# Bog-standard neural network
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME        RMSE    MAE    MPE       MAPE
# Test set -0.573257 151.378 111.414 -342.524 369.05 (de-scaled)
# RMSLE: 0.99700
nn <- neuralnet(count ~ scaled_hour + scaled_dayofweek + scaled_temp + 
                  scaled_humidity + scaled_windspeed,
                data = scaled_train.df,
                linear.output = TRUE)

nn$result.matrix
plot(nn)

nn.pred <- compute(nn, scaled_valid.df)
# compare estimated vs actual
results <- data.frame(actual = scaled_valid.df$count, prediction = nn.pred$net.result)
accuracy(results$prediction, results$actual)



# NN prediction from competition data.
# RMSLE of submitted data: 0.99700
nn.test.pred <- compute(nn, biketest.df)
nn.competition.results <- data.frame(datetime = biketest.df$datetime, count = nn.test.pred$net.result)

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(nn.competition.results, file = "output/nn_defaults_5_variables.csv", row.names=FALSE)



# Try again, with some more variables.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME      RMSE     MAE     MPE    MAPE
# Test set 1.03678 182.913 144.025 -768.79 801.252
# RMSLE of submitted data: 0.99700
nn_binaries <- neuralnet(count ~ scaled_hour + scaled_dayofweek + scaled_temp + 
                  scaled_humidity + scaled_windspeed + scaled_season +
                    senate + house + session_any,
                data = scaled_train.df, hidden = c(6, 3), rep = 3,
                linear.output = TRUE)

nn_binaries$result.matrix
plot(nn_binaries)

nn_binaries.pred <- compute(nn_binaries, scaled_valid.df)
results_nn_binaries <- data.frame(actual = scaled_valid.df$count, prediction = nn_binaries.pred$net.result)
accuracy(results_nn_binaries$prediction, results$actual)

# Prediction from competition data.
# RMSLE of submitted data: 0.99700
nn_binaries.test.pred <- compute(nn_binaries, nn_test.df)
nn_binaries.competition.results <- data.frame(datetime = biketest.df$datetime, count = nn_binaries.test.pred$net.result)

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(nn.competition.results, file = "output/nn_binaries.csv", row.names=FALSE)






# Regression tree: Scaled data  --------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#          ME       RMSE     MAE      MPE    MAPE
# Test set 0.123124 105.127 74.08 -141.174 165.475
# RMSLE of submitted data: 0.90795

# ALL IN !!!!!!!
scaled_train.df <- nn_data.df

# performing regression trees
scaled_tree_1 <- rpart(
  formula = count ~ .,
  data = scaled_train.df,
  method = "anova"
)

rpart.plot(scaled_tree_1)
plotcp(scaled_tree_1) # Maximum size/depth of tree = 17, or so it seems.

scaled_tree_2 <- rpart(
  formula = count ~ .,
  data = scaled_train.df,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(scaled_tree_2) # Maximum size/depth looks way higher here.
abline(v = 70, lty = "dashed")
abline(v = 80, lty = "dashed")

# perform a grid search
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(80, 90, 1)
)

# iterate through each minsplit and maxdepth combination.
# save each model into its own list item.
models <- list()

for (i in 1:nrow(hyper_grid)) {
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = count ~ .,
    data = scaled_train.df,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


# -- Create Optimal tree ------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
scaled_optimal_tree <- rpart(
  formula = count ~ .,
  data = scaled_train.df,
  method = "anova",
  control = list(minsplit = 16, maxdepth = 58, cp = 0.01)
)

rt.scaled.optimal.pred <- predict(scaled_optimal_tree, newdata = scaled_valid.df)
# compare estimated vs actual
accuracy(rt.scaled.optimal.pred, scaled_valid.df$count)


# -- Regression tree: scaled data - prediction from competition data. ------------------------------------
# RMSLE of submitted data: 0.90795
rt.scaled.test.optimal.pred <- predict(scaled_optimal_tree, newdata = nn_test.df)
rt.descaled.competition.results <- data.frame(datetime = biketest.df$datetime, count = rt.scaled.test.optimal.pred)

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
# write.csv(rt.descaled.competition.results, file = "output/regression_tree_scaled_data.csv", row.names=FALSE)
write.csv(rt.descaled.competition.results, file = "output/regression_tree_scaled_all_train_data.csv", row.names=FALSE)

plot(scaled_optimal_tree)
rpart.plot(scaled_optimal_tree)
prp(scaled_optimal_tree)




# -- Linear regression. Single, scaled variable --------------------
#          ME       RMSE    MAE      MPE    MAPE
# Test set 0.743597 168.015 127.788 -569.081 599.818


offhours.training.df <- subset(training.df, hour < 9 | hour > 20)
offhours.validation.df <- subset(validation.df, hour < 9 | hour > 20)
dayhours.training.df <- subset(training.df, hour < 21 & hour > 8)
dayhours.validation.df <- subset(validation.df, hour < 21 & hour > 8)



# When I split into daytime and off hours, I got RMSLE of 1.24144
# try with just daytime hours
scaled.day.lm <- lm(count ~ scaled_atemp, data = dayhours.training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.day.lm, na.action = na.pass)
accuracy(scaled.lm.train.pred, dayhours.training.df$count)

scaled.lm.valid.pred <- predict(scaled.day.lm, newdata = dayhours.validation.df, na.action = na.pass)
accuracy(scaled.lm.valid.pred, dayhours.validation.df$count)

# try with just off hours
scaled.off.lm <- lm(count ~ scaled_atemp, data = offhours.training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.off.lm, na.action = na.pass)
accuracy(scaled.lm.train.pred, offhours.training.df$count)

scaled.lm.valid.pred <- predict(scaled.off.lm, newdata = offhours.validation.df, na.action = na.pass)
accuracy(scaled.lm.valid.pred, offhours.validation.df$count)



# All hours
scaled.lm <- lm(count ~ scaled_atemp, data = training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.lm, na.action = na.pass)
accuracy(scaled.lm.train.pred, training.df$count)


# predict validation data
scaled.lm.valid.pred <- predict(scaled.lm, newdata = validation.df, na.action = na.pass)
accuracy(scaled.lm.valid.pred, validation.df$count)

# -- Predict from competition data. Remove negatives and write to CSV ------------------------------------

dayhours.test.df <- subset(biketest.df, hour < 21 & hour > 8)
offhours.test.df <- subset(biketest.df, hour < 9 | hour > 20)


## Predictions with test/competition data.
scaled.lm.day.test.pred <- predict(scaled.day.lm, newdata = dayhours.test.df, na.action = na.pass)
scaled.lm.off.test.pred <- predict(scaled.off.lm, newdata = offhours.test.df, na.action = na.pass)

day.df <- data.frame(datetime = dayhours.test.df$datetime, count = scaled.lm.day.test.pred)
off.df <- data.frame(datetime = offhours.test.df$datetime, count = scaled.lm.off.test.pred)

scaled.dayoff.pred.df <- rbind(day.df, off.df)
scaled.dayoff.pred.df <- scaled.dayoff.pred.df[order(scaled.dayoff.pred.df$datetime),]


# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(scaled.dayoff.pred.df, file = "output/scaled_lm_day_vs_offhours.csv", row.names=FALSE)




# ++ Plots -----------------------------------

# Declare some variables
boxplot.binary.colors <- c("#E69F00", "#56B4E9")
seasons <- c("Spring", "Summer", "Fall", "Winter")
days.of.week <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
weather.colors <- c("blue", "orange", "red", "black") # color per level of weather
# - weather: (categorical)
# - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog




# Leadoff Graphic: Histogram of count ----------------------
ggplot() + geom_histogram(data = bikeplot.df, aes(x = count), color = "black",
                                      fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(x = "count (hourly usage count)", y = "Frequency", title = "Histogram of hourly count (usage)") +
  geom_vline(aes(xintercept = median(bikeplot.df$count), colour = "median")) + 
  geom_vline(aes(xintercept = mean(bikeplot.df$count), colour = "mean")) +
  geom_vline(aes(xintercept = quantile(bikeplot.df$count, .90), color = "90%"))






# hour & dayofweek ----------------------------------

# Bar chart : count by dayofweek ---------
dayofweek.bar.data <- group_by(bikeplot.df, dayofweek)
dayofweek.bar.data <- summarise(dayofweek.bar.data, mean = mean(count), median = median(count))

dayofweek.bar.data <- melt(dayofweek.bar.data, id.vars = 'dayofweek')
dayofweek.bar <- ggplot() + 
  geom_bar(data = dayofweek.bar.data,
           aes(x = dayofweek, y = value, fill = variable),
           stat = 'identity', position = 'dodge', alpha = 0.7) + 
  scale_x_discrete(labels = days.of.week) +
  theme(legend.position="top") +
  labs(title = "count by dayofweek", y = "count")
  # coord_flip()
dayofweek.bar


# hour trend line data 
median.hourly.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$hour), FUN = median)
names(median.hourly.count) <- c("hour", "mediancount")
# median.weekday.hourly.count <- subset(bikeplot.df, workingday == 0, select = c("hour", "count"))
# median.weekday.hourly.count <- aggregate(median.weekday.hourly.count,
#                                          by = list(median.weekday.hourly.count$hour), FUN = median)

# Tile plot : Count: hour x day -------------------
hour.tile.data <- group_by(bikeplot.df, hour, dayofweek)
hour.tile.data <- summarise(hour.tile.data, mean_count = mean(count))

hour.day.tile <- ggplot(hour.tile.data, aes(hour, dayofweek)) +
  geom_tile(aes(fill = mean_count)) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_y_discrete(labels = days.of.week, breaks = c(7, 6, 5, 4, 3, 2, 1)) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  # theme_classic() +
  theme(legend.position="top") +
  labs(title = "Heatmap: mean_count")
hour.day.tile


# JITTERy Scatter Plot : (colored by dayofweek)
# dayofweek.colors <- c("red", "violet", "purple", "blue", "green", "yellow", "orange") # color per day
# 
# hour.scatter1 <- ggplot() +
#   # scatter plot
#   geom_point(data = bikeplot.df, aes(x = jitter(hour, 2), y = count, colour = bikeplot.df$dayofweek), 
#              pch = 20, alpha = 0.5) +
#   # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
#   scale_color_manual(values=dayofweek.colors) + 
#   labs(title = "Count by Hour (color: dayofweek) w/ median trendline", 
#        x = "Hour of Day (00-23)", y = "Count", color = "dayofweek") +
#   # line plot : median.hourly.count
#   geom_line(data = median.hourly.count, aes(x = hour, y = mediancount), size = 1)

# hour.scatter1

# JITTERy Scatter Plot : (color: workingday)
workingday.colors <- c("blue3", "darkorange2") # color per workday/not
hour.scatter2 <- ggplot() +
  # scatter plot
  geom_point(data = bikeplot.df, aes(x = jitter(hour, 2), y = count, colour = bikeplot.df$workingday), 
             pch = 16, alpha = 0.3) + 
  scale_y_sqrt() + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "count by hour w/ median trendline", subtitle = "color: workingday",
       x = "Hour of Day (00-23)", y = "Count", color = "workingday") +
  # line plot : median.hourly.count
  geom_line(data = median.hourly.count, aes(x = hour, y = mediancount), size = 1, color = "grey25") +
  # geom_line(data = median.weekday.hourly.count, aes(x = hour, y = count), size = 1) +
  theme(legend.position="top")
  # geom_smooth(data = median.hourly.count, aes(x = hour, y = mediancount), method = "auto")


# Place on grid.
grid.arrange(dayofweek.bar, hour.day.tile, hour.scatter2, ncol = 3) # , widths = c(1, 2, 2))




# dayofweek --------------------------- 
# JITTERy Scatter Plot: count by dayofweek (Holidays in red) 
holiday.colors <- c("0" = "blue", "1" = "red")
ggplot(bikeplot.df) + geom_point(aes(x = jitter(as.numeric(dayofweek), 2), y = count, colour = bikeplot.df$holiday), 
                                  pch = 16, alpha = 0.3) +
  scale_color_manual(values = holiday.colors) +
  labs(title = "Count by Day of Week (with Holidays in red)", 
       x = "Day of Week (1-7 | Sun - Sat)", 
       y = "Count", 
       color = "Holiday?")




# Temperature (atemp & temp) ----------------------------
# Count rarely low when temp is high 
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius

# make trend line data first
median.temp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$temp), FUN = median)
median.atemp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$atemp), FUN = median)
names(median.temp.count) <- c("temp", "mediancount")
names(median.atemp.count) <- c("atemp", "mediancount")



temp.histo.data <- data.frame(atemp = as.factor(bikeplot.df$atemp), 
                              temp = as.factor(bikeplot.df$temp))
ggplot(data = temp.histo.data, aes(x = temp)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Scatter Plots: Count by Temperature with median usage trendline
# atemp
atemp.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$atemp, 2), y = count),
             pch = 16, colour = "orange", alpha = 0.3) +
  # scale_y_sqrt() +
  # scale_y_log10() +
  geom_line(data = median.atemp.count, aes(x = atemp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.atemp.count, 
              aes(x = atemp, y = mediancount, color = mediancount), 
              method = "auto") +
  # se = FALSE, na.rm = TRUE, formula = y ~ poly(x, 2), method = "lm", 
  labs(x = "atemp", y = "count", title = "count by atemp - w/ median trend", subtitle = "'feels like' ºC")
atemp.scatter

# temp
temp.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, aes(x = jitter(bikeplot.df$temp, 2), y = count),
                                  pch = 16, colour = "salmon", alpha = 0.3) +
  labs(x = "temp", y = "Count", title = "Count by temp (ºC) - w/ median trend") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) +
  # scale_y_sqrt() +
  geom_line(data = median.temp.count, aes(x = temp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.temp.count, aes(x = temp, y = mediancount)) +
  stat_smooth(data = bikeplot.df, aes(x = temp, y = count), color = "red", method = gam, formula = y ~ s(x)) +
  stat_smooth(data = bikeplot.df, aes(x = temp, y = count), color = "green", method = lm, formula = y ~ x)

temp.scatter
  
# Median temp by hour of day: Line Plot
data.for.plot <- aggregate(bikeplot.df$temp, by = list(bikeplot.df$hour), FUN = median)
names(data.for.plot) <- c("hour", "mediantemp")
temp.line <- ggplot() + geom_line(data = data.for.plot, aes(x = hour, y = mediantemp),
                                  stat = "identity") + ylim(0, 41) +
  labs(title = "temp varies little through day (~ 5ºC)", y = "median temp", x = "Hour of Day (00-23)")


# Place on grid.
grid.arrange(atemp.scatter, temp.scatter, temp.line, ncol = 3)


# --------- Humidity ----------------------------
# Count by Humidity : JITTERy Scatter Plot


# make trend line data first
median.humidity.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$humidity), FUN = median)
names(median.humidity.count) <- c("humidity", "mediancount")

humidity.weather.scatter <- ggplot() +
  # scatter plot
  geom_point(data = bikeplot.df, aes(x = jitter(humidity, 2), y = count, colour = bikeplot.df$weather), 
             pch = 16, alpha = 0.3) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_y_sqrt() +
  scale_color_manual(values=weather.colors) +
  theme(legend.position="top") + 
  labs(title = "Count by humidity w/ median trendline", subtitle = "color: weather",
       x = "Humidity", y = "Count", color = "weather") +
  # line plot : median.humidity.count
  # geom_line(data = median.humidity.count, aes(x = humidity, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.humidity.count, aes(x = humidity, y = mediancount), color = "grey28") +
  stat_smooth(data = bikeplot.df, aes(x = humidity, y = count), color = "red", method = gam, formula = y ~ s(x)) +
  stat_smooth(data = bikeplot.df, aes(x = humidity, y = count), color = "green", method = lm, formula = y ~ x)


humidity.weather.scatter


# Humidity by hour Scatterplot w/ median trendline
data.for.plot <- aggregate(bikeplot.df$humidity, by = list(bikeplot.df$hour), FUN = median)
names(data.for.plot) <- c("hour", "medianhumidity")
humidity.by.hour <- ggplot() + 
  # geom_bar(data = median.hourly.count, aes(x = hour, y = mediancount / 5),
  #          colour = "grey", fill = "grey", alpha = 0.2, 
  #          stat = "identity") +
  geom_point(data = bikeplot.df, aes(x = jitter(bikeplot.df$hour, 2), y = humidity),
                                  pch = 20, colour = "seagreen3", alpha = 0.3) +
  geom_line(data = data.for.plot, aes(x = hour, y = medianhumidity), stat = "identity")
  labs(x = "Hour of Day (00-23)", y = "humidity",
       title = "Humidity by hour w/ median trendline",
       color = "medianhumidity")
humidity.by.hour

# Scatter Plot : atemp vs humidity
# atemp.humid.scatter <- ggplot() +
#   geom_point(data = bikeplot.df, aes(x = jitter(humidity, 2), y = atemp),
#              pch = 20, color = "violetred", alpha = 0.3) +
#   labs(x = "humidity", y = "atemp", title = "atemp vs humidity")
# atemp.humid.scatter

# Place on grid.
grid.arrange(humidity.weather.scatter, humidity.by.hour, ncol = 2)


# Count : Holiday vs. Not
# -------  HOLIDAY USE BOX PLOT --------------
# As a boxplot
ggplot(bikeplot.df) + geom_boxplot(aes(x = holiday, y = count), 
                                    pch = 20, color = "brown", fill = "rosybrown2", alpha = 0.4) + 
  labs(x = "holiday", y = "count", title = "count by holiday (binary)")



# ---- windspeed ----------------------------------
# Count by Wind Speed : Scatter Plot
# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# only 30 distinct values, makes this goofy.

median.windspeed.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$windspeed), FUN = median)
names(median.windspeed.count) <- c("windspeed", "mediancount")

# Count by Hour (median) : Bar chart
windspeed.bar <- ggplot(median.windspeed.count) + geom_bar(aes(x = windspeed, y = mediancount), 
                                               colour = "black", fill = "blue", 
                                               alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "median count by windspeed", x = "windspeed", y = "median count")
windspeed.bar


windspeed.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$windspeed, 6), y = count),
             pch = 20,
             # colour = bikeplot.df$season,
             # colour = bikeplot.df$hour %/% 5 + 1,
             colour = "green4",
             alpha = 0.3) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # Good with our without.
  # scale_x_log10(breaks = c(2, 4, 8, 16, 32, 64)) + 
  scale_y_sqrt() +
  # scale_color_manual(values=weather.colors) + 
  labs(x = "Wind Speed (units not provided)", y = "Count", title = "Count by Wind Speed - with median count trendline") +
  # geom_line(data = median.windspeed.count, aes(x = windspeed, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.windspeed.count, aes(x = windspeed, y = mediancount))
  

# Histogram of windspeed speed
wind.histo <- ggplot(bikeplot.df) + geom_histogram(aes(x = windspeed), color = "black",
                                               fill = "green4", alpha = 0.3, binwidth = 1) +
  labs(x = "windspeed (unknown units)", y = "Frequency", title = "Histogram: windspeed")

# Place on grid.
grid.arrange(wind.histo, windspeed.scatter, ncol=2)


# ---- CONGRESS IN SESSION... LOWER DEMAND ??? -------------
## side-by-side boxplots
# derive a column which is true if either house or senate is true.
bikeplot.df$congress_both <- ifelse(bikeplot.df$house == '1' | bikeplot.df$senate == '1', 1, 0)
bikeplot.df[,'congress_both'] <- factor(bikeplot.df[,'congress_both'])

measure.vars = c("house", "senate", "congress_both")
keeps <- c(measure.vars, "count")
congress.df <- bikeplot.df[keeps]

congress.df <- melt(congress.df, measure.vars = measure.vars)

ggplot(congress.df, aes(x = variable, y = count, fill = value)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "Congress (house or senate) in session")



# --------- GAME ON? ... INCREASED DEMAND !!!!! -------------
## side-by-side boxplots
measure.vars = c("capitals", "nationals", "united", "wizards", "sporting_event")
keeps <- c(measure.vars, "count")
sports.df <- bikeplot.df[keeps]

# tried to get: variable, value, count (as in...    nationals, 1, 250)
# but I got... count, variable(event), value (0-1)
sports.df <- melt(sports.df, measure.vars = measure.vars)

ggplot(sports.df, aes(x = variable, y = count, fill = value)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "Pro sports events & combined 'sporting_event'")


# Boxplot: sporting_event only during comparable times of day
sportinghours.train.df <- subset(bikeplot.df, (hour > 17) | (hour > 11 & hour < 16))
ggplot(sportinghours.train.df, 
       aes(x = sporting_event, y = count, fill = sporting_event)) + 
  geom_boxplot() +
  #theme_minimal() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "sporting_event: comparable times", subtitle = "(noon - 15:59 or after 17:59)")


# --------- UNIVERSITIES IN SESSION... LOWER DEMAND ??? --------- 
measure.vars = c("cua_session", "au_session", "howard_session", "session_any")
keeps <- c(measure.vars, "count")
unis.df <- bikeplot.df[keeps]

unis.df <- melt(unis.df, measure.vars = measure.vars)

ggplot(unis.df, aes(x = variable, y = count, fill = value)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "Universities in session?")


# Convert session_count to factor to avoid issues with "continuous x aesthetic" from fill = session_count
bikeplot.df[,'session_count'] <- factor(bikeplot.df[,'session_count'])
session_count.box <- ggplot(bikeplot.df, aes(x = session_count, y = count, fill = session_count)) +
  geom_boxplot() +
  theme(legend.position = "top")  +
  labs(title = "University session_count")

session_any.box.data <- bikeplot.df[c('season', 'session_any', 'count')]
session_any.box <- ggplot(session_any.box.data, aes(x = season, y = count, fill = session_any)) + 
  geom_boxplot() +
  scale_x_discrete(labels = seasons) +
  scale_fill_manual(values = boxplot.binary.colors,) +
  theme(legend.position = "top") +
  labs(title = "session_any is not a proxy for season")

# Use geom_bar instead of geom_histogram to avoid Error: StatBin requires a 
# continuous x variable: the x variable is discrete. Perhaps you want stat="count"?
session_count.histo <- ggplot(data = bikeplot.df, (aes(x = session_count))) +
  geom_bar(fill = "slateblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of session_count")

# Place on grid.
grid.arrange(session_count.box, session_any.box, session_count.histo, ncol=3)




# --------- WEATHER AND SEASON --------- 
## side-by-side boxplots

weather.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = weather, y = count), pch = 20, color = "black", fill = "blue", alpha = 0.4) + 
  labs(x = "weather (1-4)", y = "Count", title = "Count by weather",
       subtitle = "4 = Heavy Rain + Ice Pellets\n + Thunderstorm + Mist, Snow + Fog")

season.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = season, y = count), pch = 20, color = "black", fill = "green", alpha = 0.4) + 
  scale_x_discrete(labels = seasons) +
  labs(y = "count", title = "Count by season") #, subtitle = "1 = Spr, 2 = Sum, 3 = Fall, 4 = Win")

weather.histo <- ggplot(bikeplot.df, (aes(x = weather))) +
  geom_bar(color = "black", alpha = 0.5, fill = "blue") +
  labs(title = "Histogram of weather")

# Place on grid.
grid.arrange(weather.box, weather.histo, season.box, ncol=3)
