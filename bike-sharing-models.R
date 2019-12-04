# -- Models ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

# Run `bike-sharing.R` first.
# That's where the library() calls and CSV reads live.




# ++ Partition training | validation ----------

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

