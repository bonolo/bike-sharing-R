# -- Models ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

# Run `bike-sharing-setup.R` first.
# That's where the library() calls and CSV reads live.


# User-defined functions.

# Predictions with negative values don't work with RMSLE (selected evaluation statistic)
negative_to_zero <- function(predictions) {
  predictions[predictions < 0] <- 0
  return(predictions)
}


# Predict scoring set with model and saved to relative file_name.
predict_scoring_set <- function(model_to_score, file_name) {
  # file_name e.g. "output/nn_defaults_5_variables.csv"
  pred <- predict(model_to_score, newdata = biketest.df, na.action = na.pass)
  pred <- negative_to_zero(pred)
  # write submission in kaggle format
  # datetime,count
  # 2011-01-20 00:00:00,0
  write.csv(data.frame(datetime = biketest.df$datetime, count = pred),
            file = file_name, row.names = FALSE)
}
  


# -- Keep only variables we would use for predictions. ----------

# Skipping stuff like casual/registered counts, individual sporting events/calendars.
# Put these in a data frames used to build models.
keeps <- c("count", "hour", "dayofweek", "month", "is_daylight", "season",
           "holiday", "workingday", "weather", "temp", "temp_squared", "atemp", "humidity", 
           "windspeed", "house", "senate", "sporting_event", "session_any",
           "scaled_hour", "scaled_dayofweek", "scaled_month", "scaled_season", "scaled_weather", 
           "scaled_temp", "scaled_temp_squared", "scaled_atemp", "scaled_humidity", "scaled_windspeed")
biketrain.df <- subset(bikeall.df, train == 1)
biketrain.df <- biketrain.df[keeps]

# Take out weather = 4. There are only 3 of these observations, which wreaks havoc with modeling.
# Many times, I get "Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# factor weather has new levels 4"
# biketrain.df$weather <- as.character(biketrain.df$weather)
# biketrain.df$weather[biketrain.df$weather =="4"] <- "3"
# biketrain.df$weather <- as.factor(biketrain.df$weather)



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
# training: RMSlE 1.29602
pred_t <- predict(bike.lm, na.action = na.pass)
forecast::accuracy(pred_t, mlr_train.df$count)
# Metrics::rmsle(actual, predicted)
pred_t <- negative_to_zero(pred_t)
rmsle(mlr_train.df$count, pred_t)

# validation: RMSLE 1.2773
pred_v <- predict(bike.lm, newdata = validation.df, na.action = na.pass)
forecast::accuracy(pred_v, validation.df$count)
pred_v <- negative_to_zero(pred_v)
rmsle(mlr_valid.df$count, pred_v)

summary(bike.lm)



# -- GAM regression. Single variable: temp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(temp), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)

# training: RMSLE 1.43698
gam.train.pred <- negative_to_zero(gam.train.pred)
rmsle(training.df$count, gam.train.pred)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.42552
gam.valid.pred <- negative_to_zero(gam.valid.pred)
rmsle(validation.df$count, gam.valid.pred)


# -- GAM regression. Single variable: atemp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(atemp), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)

# training: RMSLE 1.44657
gam.train.pred <- negative_to_zero(gam.train.pred)
rmsle(training.df$count, gam.train.pred)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.43655
gam.valid.pred <- negative_to_zero(gam.valid.pred)
rmsle(validation.df$count, gam.valid.pred)



# -- GAM regression. Single variable: humidity -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(humidity), data = training.df)
# predict training data and check accuracy
gam.train.pred <- predict(bike.gam, na.action = na.pass)

# training: RMSLE 1.47188
gam.train.pred <- negative_to_zero(gam.train.pred)
rmsle(training.df$count, gam.train.pred)

# predict validation data and check accuracy
gam.valid.pred <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.45779
gam.valid.pred <- negative_to_zero(gam.valid.pred)
rmsle(validation.df$count, gam.valid.pred)



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
temp.lm <- lm(count ~ temp, data = training.df)

# predict training data and check accuracy
temp.lm.train.pred <- predict(temp.lm, na.action = na.pass)

# training: RMSLE 1.44339
temp.lm.train.pred <- negative_to_zero(temp.lm.train.pred)
rmsle(training.df$count, temp.lm.train.pred)

# predict validation data and check accuracy
temp.lm.valid.pred <- predict(temp.lm, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.43295
temp.lm.valid.pred <- negative_to_zero(temp.lm.valid.pred)
rmsle(validation.df$count, temp.lm.valid.pred)




# -- LM regression. Single variable: humidity ---------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
humidity.lm <- lm(count ~ humidity, data = training.df)

# predict training data and check accuracy
humidity.lm.train.pred <- predict(humidity.lm, na.action = na.pass)

# training: RMSLE 1.48162
humidity.lm.train.pred <- negative_to_zero(humidity.lm.train.pred)
rmsle(training.df$count, humidity.lm.train.pred)

# predict validation data and check accuracy
humidity.lm.valid.pred <- predict(humidity.lm, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.46728
humidity.lm.valid.pred <- negative_to_zero(humidity.lm.valid.pred)
rmsle(validation.df$count, humidity.lm.valid.pred)



# -- linear regression with stepwise variable selection ----------------------------
# Throw everything at it.
# Scored RMSLE: 1.26428
# An earlier version scored RMSLE = 1.64349 without is_daylight, month, scaled, etc.

#### Table 6.6
# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both"
bike.lm <- lm(count ~ ., data = training.df, na.action = na.exclude)
bike.step.lm <- step(bike.lm, direction = "both")
summary(bike.step.lm)

bike.step.lm.pred <- predict(bike.step.lm, training.df)

# training: RMSLE 1.19448
bike.step.lm.pred <- negative_to_zero(bike.step.lm.pred)
rmsle(training.df$count, bike.step.lm.pred)

# predict validation data
bike.step.lm.valid.pred <- predict(bike.step.lm, validation.df)

# validation: RMSLE 1.18628
bike.step.lm.valid.pred <- negative_to_zero(bike.step.lm.valid.pred)
rmsle(validation.df$count, bike.step.lm.valid.pred)

# Predict scoring set
predict_scoring_set(bike.step.lm, "output/lm_stepwise_selection.csv")


# residuals <- bike.step.lm.pred - validation.df$count
# hist(residuals, breaks = 50, xlab = "residual (predicted - actual)")



# Regression tree  --------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# courtesy of tutorial: http://uc-r.github.io/regression_trees

# unscaled variables
vars_to_use <- c('count', 'hour', 'dayofweek', 'month', 'is_daylight', 'season', 
                 'workingday', 'temp', 'humidity', 'windspeed', 
                  'house', 'senate', 'sporting_event', 'session_any')

rt_train.df <- subset(training.df, select = vars_to_use)
rt_valid.df <- subset(validation.df, select = vars_to_use)


# performing regression trees
m1 <- rpart(
  formula = count ~ .,
  data = rt_train.df,
  method = "anova"
)

rpart.plot(m1)
plotcp(m1) # Maximum size/depth of tree = 16, or so it seems.

m2 <- rpart(
  formula = count ~ .,
  data = rt_train.df,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(m2) # Maximum size/depth looks way higher here.
abline(v = 16, lty = "dashed")
abline(v = 75, lty = "dashed")

# perform a grid search
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(70, 80, 1)
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
    data = rt_train.df,
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
  data = rt_train.df,
  method = "anova",
  control = list(minsplit = 16, maxdepth = 71, cp = 0.01)
)




rt.optimal.pred <- predict(optimal_tree, newdata = validation.df)
# forecast::accuracy(rt.optimal.pred, validation.df$count)
# validation: RMSLE 0.864251
rmsle(validation.df$count, rt.optimal.pred)


rpart.plot(optimal_tree)


# -- Predict from competition data. Remove negatives and write to CSV ------------------------------------
# RMSLE: 0.93428
# An earlier version scored RMSLE = 1.02692 without is_daylight, month, scaled, etc.

## Predictions with test/competition data.
# rt.optimal.pred <- predict(optimal_tree, newdata = biketest.df, na.action = na.pass)
predict_scoring_set(optimal_tree, "output/regression_tree_optimal.csv")


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
vars_to_use <- c("count", "scaled_hour", "scaled_dayofweek", "scaled_month", "scaled_season",
                 "scaled_weather", "house", "senate", "scaled_temp", "scaled_humidity",
                 "scaled_windspeed", "session_any")
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

# Bog-standard neural network. 6 predictors
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE: 0.99141
# An earlier version scored RMSLE = 0.99700 before adding scaled_month
nn <- neuralnet(count ~ scaled_hour + scaled_dayofweek + scaled_month + scaled_temp + 
                  scaled_humidity + scaled_windspeed,
                data = scaled_train.df,
                linear.output = TRUE)

nn$result.matrix
plot(nn)

# Predict validation data set
nn.pred <- compute(nn, scaled_valid.df)
# validation: RMSLE 0.943666
rmsle(scaled_valid.df$count, nn.pred$net.result)


# NN prediction from competition data.
nn.test.pred <- compute(nn, biketest.df)
nn.competition.results <- data.frame(datetime = biketest.df$datetime, count = nn.test.pred$net.result)

predict_scoring_set(nn, "output/nn_defaults_6_variables.csv")



# Try again, with some more variables.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
forecast::accuracy(results_nn_binaries$prediction, results$actual)

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
# RMSLE 0.89715 - no improvements when adding month, is_daylight, etc.
# An earlier version scored RMSLE = 0.90795 60% training data

# ALL IN !!!!!!! Use all data with no validation set.
scaled_train.df <- nn_data.df

# performing regression trees
scaled_tree_1 <- rpart(
  formula = count ~ .,
  data = scaled_train.df,
  method = "anova"
)

rpart.plot(scaled_tree_1)
plotcp(scaled_tree_1) # Maximum size/depth of tree = 16, or so it seems.

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
  control = list(minsplit = 12, maxdepth = 89, cp = 0.01)
)

# Predict against validation data
rt.scaled.optimal.pred <- predict(scaled_optimal_tree, newdata = scaled_valid.df)
# validation: RMSLE 0.860262
rmsle(scaled_valid.df$count, rt.scaled.optimal.pred)


# -- Regression tree: scaled data - prediction from competition data. ------------------------------------
# RMSLE of submitted data: 0.89715
rt.scaled.test.optimal.pred <- predict(scaled_optimal_tree, newdata = nn_test.df)
# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(data.frame(datetime = biketest.df$datetime, count = rt.scaled.test.optimal.pred),
          file = "output/regression_tree_scaled_all_train_data.csv", row.names = FALSE)


plot(scaled_optimal_tree)
rpart.plot(scaled_optimal_tree)
prp(scaled_optimal_tree)




# -- Linear regression. Single, scaled variable. Partition dayhours & offhours -----------
# RMSLE 1.24144 (scored)

offhours.training.df <- subset(training.df, hour < 9 | hour > 20)
offhours.validation.df <- subset(validation.df, hour < 9 | hour > 20)
dayhours.training.df <- subset(training.df, hour < 21 & hour > 8)
dayhours.validation.df <- subset(validation.df, hour < 21 & hour > 8)

# try with just daytime hours
scaled.day.lm <- lm(count ~ scaled_atemp, data = dayhours.training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.day.lm, na.action = na.pass)

scaled.lm.valid.pred <- predict(scaled.day.lm, newdata = dayhours.validation.df, na.action = na.pass)

# try with just off hours
scaled.off.lm <- lm(count ~ scaled_atemp, data = offhours.training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.off.lm, na.action = na.pass)

scaled.lm.valid.pred <- predict(scaled.off.lm, newdata = offhours.validation.df, na.action = na.pass)

# All hours
scaled.lm <- lm(count ~ scaled_atemp, data = training.df, na.action = na.exclude)
scaled.lm.train.pred <- predict(scaled.lm, na.action = na.pass)

# predict validation data
scaled.lm.valid.pred <- predict(scaled.lm, newdata = validation.df, na.action = na.pass)
forecast::accuracy(scaled.lm.valid.pred, validation.df$count)

scaled.lm.valid.pred <- negative_to_zero(scaled.lm.valid.pred)
rmsle(validation.df$count, scaled.lm.valid.pred)

# Predict from competition data.
# partition biketest.df
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

