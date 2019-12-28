# -- Models ---------------
# Kevin F Cullen

# Run `bike-sharing-setup.R` first.
# That's where the library() calls and CSV reads live.
source('bike-sharing-setup.R')


# -- User-defined functions.

# Predictions with negative values don't work with RMSLE (selected evaluation statistic)
negative_to_zero <- function(predictions) {
  predictions[predictions < 0] <- 0
  return(predictions)
}


# Predict scoring set with model and saved to relative file_name.
predict_scoring_set <- function(model_to_score, file_name, newdata = biketest.df) {
  # file_name e.g. "output/nn_defaults_5_variables.csv"
  pred <- predict(model_to_score, newdata = newdata, na.action = na.pass)
  pred <- negative_to_zero(pred)
  # write submission in kaggle format
  # datetime,count
  # 2011-01-20 00:00:00,0
  write.csv(data.frame(datetime = newdata$datetime, count = pred),
            file = file_name, row.names = FALSE)
}

# courtesy of tutorial: http://uc-r.github.io/regression_trees
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


# -- Keep only variables we would use for predictions. ----------

# Skipping stuff like casual/registered counts, individual sporting events/calendars.
# Put these in a data frame.
keeps <- c("count", "hour", "dayofweek", "month", "is_daylight", "peak", 'peak_alt', "season",
           "holiday", "workingday", "weather", "temp", "temp_squared", "atemp", "humidity", 
           "windspeed", "house", "senate", "congress_both", "sporting_event", "session_any",
           "scaled_hour", "scaled_dayofweek", "scaled_month", "scaled_season", "scaled_weather", 
           "scaled_temp", "scaled_temp_squared", "scaled_atemp", "scaled_humidity", "scaled_windspeed",
           "house_num", "senate_num", "session_any_num")
biketrain.df <- subset(bikeall.df, train == 1)
biketrain.df <- subset(biketrain.df, select = keeps)
rm(keeps)


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
# RMSLE 1.31020 w/out humidity.
# RMSLE 1.28159 with humidity.
# LOTS of negative predictions to remove.

vars_to_use <- c('count', 'hour', 'dayofweek', 'season', 'workingday', 'humidity', # weather, 
                 'temp', 'windspeed', 'house', 'senate', 'session_any')

mlr_train.df <- subset(training.df, select = vars_to_use)
mlr_valid.df <- subset(validation.df, select = vars_to_use)

bike.lm <- lm(count ~ ., data = mlr_train.df, na.action = na.exclude)
# training: RMSlE 1.29602
pred.train <- predict(bike.lm, na.action = na.pass)
pred.train <- negative_to_zero(pred.train)
# Metrics::rmsle(actual, predicted)
rmsle(mlr_train.df$count, pred.train)

# validation: RMSLE 1.2773
pred.valid <- predict(bike.lm, newdata = validation.df, na.action = na.pass)
pred.valid <- negative_to_zero(pred.valid)
rmsle(mlr_valid.df$count, pred.valid)

# remove variables
rm(list = c('mlr_train.df', 'mlr_valid.df', 'vars_to_use', 'pred.train', 'pred.valid'))
# summary(bike.lm)

predict_scoring_set(bike.lm, 'output/test-function.csv')


# -- Generalized additive model. Single variable: temp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(temp), data = training.df)

# predict training data and check accuracy
pred.train <- predict(bike.gam, na.action = na.pass)
# training: RMSLE 1.43698
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data and check accuracy
pred.valid <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.42552
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))


# -- Generalized additive model. Single variable: atemp -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(atemp), data = training.df)

# predict training data and check accuracy
pred.train <- predict(bike.gam, na.action = na.pass)
# training: RMSLE 1.44657
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data and check accuracy
pred.valid <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.43655
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))


# -- Generalized additive model. Single variable: humidity -------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
bike.gam <- gam(count ~ s(humidity), data = training.df)

# predict training data and check accuracy
pred.train <- predict(bike.gam, na.action = na.pass)
# training: RMSLE 1.47188
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data and check accuracy
pred.valid <- predict(bike.gam, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.45779
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))



# -- LM regression. Single variable: temp ---------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
temp.lm <- lm(count ~ temp, data = training.df)

# predict training data and check accuracy
pred.train <- predict(temp.lm, na.action = na.pass)

# training: RMSLE 1.44339
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data and check accuracy
pred.valid <- predict(temp.lm, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.43295
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))



# -- LM regression. Single variable: humidity ---------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
humidity.lm <- lm(count ~ humidity, data = training.df)

# predict training data and check accuracy
pred.train <- predict(humidity.lm, na.action = na.pass)

# training: RMSLE 1.48162
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data and check accuracy
pred.valid <- predict(humidity.lm, newdata = validation.df, na.action = na.pass)

# validation: RMSLE 1.46728
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))



# -- linear regression with stepwise variable selection ----------------------------
# Throw everything at it.
# Scored RMSLE: 1.26428
# An earlier version scored RMSLE = 1.64349 without is_daylight, month, scaled, etc.

#### Table 6.6 from the textbook
# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both"
bike.lm <- lm(count ~ ., data = training.df, na.action = na.exclude)
bike.step.lm <- step(bike.lm, direction = "both")
summary(bike.step.lm)

pred.train <- predict(bike.step.lm, training.df)

# training: RMSLE 1.19448
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data
pred.valid <- predict(bike.step.lm, validation.df)

# validation: RMSLE 1.18628
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))

# Predict scoring set
predict_scoring_set(bike.step.lm, "output/lm_stepwise_selection.csv")

# residuals <- bike.step.lm.pred - validation.df$count
# hist(residuals, breaks = 50, xlab = "residual (predicted - actual)")



# -- Polynomial regression -------------------------------
# polynomial.temp.lm <- lm(data = training.df, count ~ poly(temp, 2, raw = TRUE))
polynomial.temp.lm <- lm(data = training.df, count ~ poly(temp, 2, raw = TRUE))
summary(polynomial.temp.lm)

pred.train <- predict(polynomial.temp.lm, training.df)

# training: RMSLE 1.43911
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data
pred.valid <- predict(polynomial.temp.lm, validation.df)

# validation: RMSLE 1.42811
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))



# -- Log transformation regression -------------------------------
log.temp.lm <- lm(data = training.df, count ~ log(temp))
summary(log.temp.lm)

pred.train <- predict(log.temp.lm, training.df)

# training: RMSLE 1.48783
pred.train <- negative_to_zero(pred.train)
rmsle(training.df$count, pred.train)

# predict validation data
pred.valid <- predict(log.temp.lm, validation.df)

# validation: RMSLE 1.47595
pred.valid <- negative_to_zero(pred.valid)
rmsle(validation.df$count, pred.valid)

# remove variables
rm(list = c('pred.train', 'pred.valid'))



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

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)


# -- Create Optimal tree ------------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE: 0.93428
# An earlier version scored RMSLE = 1.02692 without is_daylight, month, scaled, etc.
optimal_tree <- rpart(
  formula = count ~ .,
  data = rt_train.df,
  method = "anova",
  control = list(minsplit = 16, maxdepth = 71, cp = 0.01)
)

pred.valid <- predict(optimal_tree, newdata = validation.df)
rmsle(validation.df$count, pred.valid)

rpart.plot(optimal_tree)

# remove variables
rm(list = c('rt_train.df', 'rt_valid.df', 'vars_to_use', 'pred.valid'))

## Predictions with test/competition data.
predict_scoring_set(optimal_tree, "output/regression_tree_optimal.csv")



# -- Bagging ???? --------------------------------------
# http://uc-r.github.io/regression_trees#bag
# See Chapter 13 for further details on bagging. ---


# -- boosted trees???



# -- random forest --------------------------------------

vars_to_use <- c('count', 'hour', 'month', 'is_daylight', 'workingday', 'atemp', 'senate',
                 'session_any')
# RMSLE was worse (.71) when using peak_alt instead of is_daylight
# vars_to_use <- c('count', 'hour', 'month', 'peak_alt', 'workingday', 'atemp', 'senate',
#                  'session_any')
rf_train.df <- subset(training.df, select = vars_to_use)
rf_valid.df <- subset(validation.df, select = vars_to_use)

rf <- randomForest(count ~ ., data = rf_train.df)

# scored RMSLE: 0.54403 <--------------------------------------------------
# scored RMSLE: 0.54590 with mtry = 7
# ('count', 'hour', 'month', 'is_daylight', 'workingday', 'atemp', 'senate', 'session_any')
rf <- randomForest(count ~ ., data = rf_train.df, ntree = 500, 
                   # mtry = 4, nodesize = 5, importance = TRUE)
                   mtry = 7, importance = TRUE)

# training: predict and check RMSLE
pred.train <- predict(rf, rf_train.df)
rmsle(training.df$count, pred.train)

# validation: predict and check RMSLE
pred.valid <- predict(rf, rf_valid.df)
rmsle(validation.df$count, pred.valid)

varImpPlot(rf) # , type = 1)

# Courtesy https://uc-r.github.io/random_forests
plot(rf)
# number of trees with lowest MSE
which.min(rf$mse)
# RMSE of this optimal random forest
sqrt(rf$mse[which.min(rf$mse)])


# remove variables
rm(list = c('rf_train.df', 'rf_valid.df', 'vars_to_use', 'pred.train', 'pred.valid'))

# Predict scoring set
predict_scoring_set(rf, "output/rf_ntree_500.csv")

summary(rf)



# -- Neural Network ------------------------------------
# https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/
# more complex: https://datascienceplus.com/fitting-neural-network-in-r/

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
                data = training.df,
                linear.output = TRUE)

nn$result.matrix
plot(nn)

# Predict validation data set
pred.valid <- compute(nn, validation.df)
# validation: RMSLE 0.943854
rmsle(validation.df$count, pred.valid$net.result)

# remove variables
rm(list = c('pred.valid'))

# NN prediction from competition data.
predict_scoring_set(nn, "output/nn_defaults_6_variables.csv")



# Try again, with some more variables.
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE: 1.26794
# Withough scaled month, this goes down to 1.58455

nn_binaries <- neuralnet(count ~ scaled_hour + scaled_dayofweek + scaled_temp + # scaled_month +
                           scaled_humidity + scaled_windspeed + scaled_season +
                           senate_num + house_num + session_any_num,
                         data = training.df, hidden = c(6, 3), rep = 3,
                         linear.output = TRUE)

nn_binaries$result.matrix
plot(nn_binaries)

# Predict validation set
pred.valid <- compute(nn_binaries, validation.df)
# validation: RMSLE 1.55724
rmsle(validation.df$count, pred.valid$net.result)

# remove variables
rm(list = c('pred.valid'))

# Prediction from competition data.
nn_binaries.test.pred <- compute(nn_binaries, biketest.df)
nn_binaries.competition.results <- data.frame(datetime = biketest.df$datetime, count = nn_binaries.test.pred$net.result)

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(nn_binaries.competition.results, file = "output/nn_binaries_10.csv", row.names=FALSE)



# Regression tree: Scaled data  --------------
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# RMSLE 0.89715 - no improvements when adding month, is_daylight, etc.
# An earlier version scored RMSLE = 0.90795 60% training data

# ALL IN !!!!!!! Use all data with no validation set.

# performing regression trees
scaled_tree_1 <- rpart(
  formula = count ~ .,
  data = biketrain.df,
  method = "anova"
)

rpart.plot(scaled_tree_1)
plotcp(scaled_tree_1) # Maximum size/depth of tree = 16, or so it seems.

scaled_tree_2 <- rpart(
  formula = count ~ .,
  data = biketrain.df,
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
    data = biketrain.df,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
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
  data = biketrain.df,
  method = "anova",
  control = list(minsplit = 12, maxdepth = 89, cp = 0.01)
)

# Predict against validation data
pred.valid <- predict(scaled_optimal_tree, newdata = validation.df)
# validation: RMSLE 0.860262
rmsle(validation.df$count, pred.valid)


# Regression tree: scaled data - prediction from competition data. --------------
# RMSLE of scored data: 0.89715
predict_scoring_set(scaled_optimal_tree, "output/regression_tree_scaled_all_train_data.csv")


plot(scaled_optimal_tree)
rpart.plot(scaled_optimal_tree)
prp(scaled_optimal_tree)


# remove variables
rm(list = c('hyper_grid', 'models', 'pred.valid'))



# ++ Ensemble models. Partitioned by peak & offpeak -----------
# or is_daylight 1, 0


# -- Linear regression. Single, scaled variable. Partition peak & offpeak -----------
# RMSLE 1.24144 (scored)
# RMSLE 1.68000 (scored) using daylight instead of peak/offpeak

# try with just daytime hours
scaled.peak.lm <- lm(count ~ scaled_atemp, data = training.df[training.df$peak == TRUE,], na.action = na.exclude)

# stepwise selection version
scaled.stepwise.peak.lm <- lm(count ~., data = training.df[training.df$peak == TRUE,], na.action = na.exclude)
scaled.stepwise.peak.lm <- step(scaled.stepwise.peak.lm, direction = "both")
summary(scaled.stepwise.peak.lm)

# training
pred.peak.train <- predict(scaled.peak.lm, na.action = na.pass)
# validation
pred.peak.valid <- predict(scaled.peak.lm, newdata = validation.df[validation.df$peak == TRUE,], na.action = na.pass)
pred.peak.stepwise.valid <- predict(scaled.stepwise.peak.lm, newdata = validation.df[validation.df$peak == TRUE,], na.action = na.pass)
# is_daylight == 1 validation RMSLE: 0.814235
# peak/offpeak validation RMSLE: 0.602574
# peak/offpeak validation RMSLE: 0.593473 (GAM)
# stepwise RMSLE: 0.733343
rmsle(validation.df[validation.df$peak == TRUE,]$count, pred.peak.valid)
# stepwise version
pred.peak.stepwise.valid <- negative_to_zero(pred.peak.stepwise.valid)
rmsle(validation.df[validation.df$peak == TRUE,]$count, pred.peak.stepwise.valid)



# try with just off hours
scaled.offpeak.lm <- lm(count ~ scaled_atemp, data = training.df[training.df$peak == FALSE,], na.action = na.exclude)

# training
pred.offpeak.train <- predict(scaled.offpeak.lm, na.action = na.pass)
# validation
pred.offpeak.valid <- predict(scaled.offpeak.lm, newdata = validation.df[validation.df$peak == FALSE,], na.action = na.pass)
# is_daylight == 0 validation RMSLE: 1.6016
# peak/offpeak validation RMSLE: 1.62375
# peak/offpeak validation RMSLE: 1.62431 (GAM)
rmsle(validation.df[validation.df$peak == FALSE,]$count, pred.offpeak.valid)

# Predict test/scoring dataset
pred.peak.score <- predict(scaled.peak.lm, newdata = biketest.df[biketest.df$peak == TRUE,], na.action = na.pass)
pred.offpeak.score <- predict(scaled.offpeak.lm, newdata = biketest.df[biketest.df$peak == FALSE,], na.action = na.pass)
# Make two dataframes
peak.df <- data.frame(datetime = biketest.df[biketest.df$peak == TRUE,]$datetime, count = pred.peak.score)
offpeak.df <- data.frame(datetime = biketest.df[biketest.df$peak == FALSE,]$datetime, count = pred.offpeak.score)
# Combine and sort by datetime
pred.peakoffpeak.df <- rbind(peak.df, offpeak.df)
pred.peakoffpeak.df <- pred.peakoffpeak.df[order(pred.peakoffpeak.df$datetime),]


# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(pred.peakoffpeak.df, file = "output/scaled_lm_peak_vs_offpeak.csv", row.names=FALSE)


# remove variables
rm(list = c('offpeak.df', 'peak.df', 'pred.peakoffpeak.df', 'pred.offpeak.score', 'pred.offpeak.train', 
            'pred.offpeak.valid',
            'pred.peak.score', 'pred.peak.stepwise.valid', 'pred.peak.train', 'pred.peak.valid'))


# -- Combined GAM and regression tree -----------
# RMSLE: 0.89011
# I pushed the RMSLE down to 0.79787 by using the 'peak_alt' 
# (day/hour combinations when usage is typically above median)

# So I don't have to rewrite everything...
# training.df$peak <- training.df$peak_alt
# validation.df$peak <- validation.df$peak_alt
# biketest.df$peak <- biketest.df$peak

# GAM for peak time
scaled.peak.gam <- gam(count ~ s(atemp), data = training.df[training.df$peak == TRUE,])
summary(scaled.peak.gam)

# training
pred.peak.gam <- predict(scaled.peak.gam, na.action = na.pass)
# validation: RMSLE 0.593473
pred.peak.gam.valid <- predict(scaled.peak.gam, newdata = validation.df[validation.df$peak == TRUE,], na.action = na.pass)
rmsle(validation.df[validation.df$peak == TRUE,]$count, pred.peak.gam.valid)


# Regression tree: Just off hours / nighttime

# performing regression trees
night_tree <- rpart(
  formula = count ~ .,
  data = training.df[training.df$peak == FALSE,],
  method = "anova"
)

rpart.plot(night_tree)
plotcp(night_tree) # Maximum size/depth of tree = 8, or so it seems.

night_tree_2 <- rpart(
  formula = count ~ .,
  data = training.df[training.df$peak == FALSE,],
  method = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(night_tree_2) # Maximum size/depth looks way higher here.
abline(v = 20, lty = "dashed")
abline(v = 30, lty = "dashed")

# perform a grid search
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(25, 35, 1)
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
    data = training.df[training.df$peak == FALSE,],
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
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
night_optimal_tree <- rpart(
  formula = count ~ .,
  data = training.df[training.df$peak == FALSE,],
  method = "anova",
  # control = list(minsplit = 19, maxdepth = 29, cp = 0.01)
  control = list(minsplit = 5, maxdepth = 31, cp = 0.01)
)

rpart.plot(night_optimal_tree)

# Predict against validation data
rt.night.optimal.pred <- predict(night_optimal_tree, newdata = validation.df[validation.df$peak == FALSE,])
# validation: RMSLE 1.07674
rmsle(validation.df[validation.df$peak == FALSE,]$count, rt.night.optimal.pred)


# Predict Test / Scoring data
# GAM model - peak
pred.peak.gam.test <- predict(scaled.peak.gam, newdata = biketest.df[biketest.df$peak == TRUE,], na.action = na.pass)
pred.peak.gam.test <- negative_to_zero(pred.peak.gam.test)
# Regression tree - offpeak
pred.offpeak.rt.test <- predict(night_optimal_tree, newdata = biketest.df[biketest.df$peak == FALSE,], na.action = na.pass)

gam.peak.df <- data.frame(datetime = biketest.df[biketest.df$peak == TRUE,]$datetime, count = pred.peak.gam.test)
rt.night.df <- data.frame(datetime = biketest.df[biketest.df$peak == FALSE,]$datetime, count = pred.offpeak.rt.test)
mixed.models.df <- rbind(gam.peak.df, rt.night.df)

write.csv(mixed.models.df, file = "output/gam+regression_tree.csv", row.names=FALSE)


# remove variables
rm(list = c('hyper_grid', 'models', 'mixed.models.df', 'gam.peak.df', 'rt.night.df',
            'pred.peak.gam', 'pred.peak.gam.test', 'pred.peak.gam.valid',
            'rt.night.optimal.pred'))
