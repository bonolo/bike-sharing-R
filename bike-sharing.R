setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(pastecs)
library(reshape2)
library(forecast)
# library(Metrics)

options(scipen = 100, digits = 6)

# pastecs library
# - better (per Leo) summary descriptive statistics
# 
# stat.desc() : descriptive statistics from pastecs library
# - displays results in scientific notation
# - fix this with options(scipen = 100, digits = 4)


# --------- Load CSV file. MySQL export is in one file, with a binary flag ---------------
# 6493 test.csv
# 10886 train.csv
# 17379 total

bikeall.df <- read.csv("csv-inputs/kaggle_data_plus.csv", na.strings = "\\N", header = TRUE)
# create id column
bikeall.df$id <- seq.int(nrow(bikeall.df))

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

# Dataframe to build predictions for contest submission
biketest.df <- subset(bikeall.df, train == 0)

# All the vars for plotting. Using train == 1 because we need `count`
bikeplot.df <- subset(bikeall.df, train == 1)

# Keep only variables we would use for predictions.
# Skipping stuff like casual/registered counts, individual sporting events/calendars.
# Put these in a data frames used to build models.
keeps <- c("id", "count", "hour", "dayofweek", "season", "holiday", "workingday", 
           "weather", "temp", "temp_squared", "atemp", "humidity", 
           "windspeed", "house", "senate", "sporting_event", "session_any")
biketrain.df <- bikeplot.df[keeps]



# -------------- Data shape & summary ----------------------------
dim(bikeall.df)
head(bikeall.df)

stat.desc(bikeall.df)

summary(bikeall.df)
median(bikeall.df$count)
bikeall.df[,c("datetime","count","nationals")]

str(bikeall.df)
describe(bikeall.df)
describe(bikeall.df$count)

describe(as.factor(bikeall.df$temp))
describe(as.factor(bikeall.df$atemp))
describe(as.factor(bikeall.df$humidity))

glimpse(bikeall.df)


# --------- Models ----------------------------


# randomly generate training and validation sets
set.seed(7)  # set seed for reproducing the partition

ss <- sample(1:2, size = nrow(biketrain.df), replace = TRUE, prob = c(0.6, 0.4))
training.df = biketrain.df[ss==1,]
validation.df = biketrain.df[ss==2,]

# --- "Multiple linear regression". Adapted from textbook. My best-guess 10 variables. ----------
# RMSLE 1.31020 w/out humidity. ME 1.26723 RMSE 141.212
# RMSLE 1.28159 with humidity. ME 0.93598 RMSE 137.619
# LOTS of negative predictions to remove.

vars_to_use <- c('count', 'hour', 'dayofweek', 'season', 'workingday', 'humidity', # weather, 
                 'temp', 'windspeed', 'house', 'senate', 'session_any')

mlr_train.df <- subset(training.df,
                   select = vars_to_use)
mlr_valid.df <- subset(validation.df,
                       select = vars_to_use)

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




# -- linear regression with stepwise variable selection ----------------------------
# RMSLE 1.64349. Bummer. ME 1.607 RMSE 136.839

#### Table 6.6
# use step() to run stepwise regression.
# set directions = to either "backward", "forward", or "both"
bike.lm <- lm(count ~ ., data = training.df, na.action = na.exclude)
bike.step.lm <- step(bike.lm, direction = "both")
summary(bike.step.lm)

# Wow... 3 of my derived variables and 2 of my added/created variables !!!
# Call: lm(formula = count ~ id + hour + dayofweek + season + weather + 
     # temp_squared + atemp + humidity + windspeed + house + session_any, 
   # data = training.df, na.action = na.exclude)

bike.step.lm.pred <- predict(bike.step.lm, training.df)
# validation...  ME 1.607 RMSE 136.839
accuracy(bike.step.lm.pred, validation.df$count) 


residuals <- bike.step.lm.pred - validation.df$count
hist(residuals, breaks = 50, xlab = "residual (predicted - actual)")







# +++ How do I do a regression tree?  --------------------
# http://uc-r.github.io/regression_trees

# performing regression trees
library(rpart)
library(rpart.plot)

m1 <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova"
)

rpart.plot(m1)
plotcp(m1) # Maximum size/depth of tree = 17, or so it seems.

m2 <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova",
  control = list(cp = 0, xval = 10)
)

plotcp(m2) # Maximum size/depth looks way higher here.
abline(v = 17, lty = "dashed")
abline(v = 30, lty = "dashed")

# perform a grid search
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(20, 30, 1)
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


# -- Create Optimal tree -------------
# RMSLE of submitted data: 1.02692. ME: -2.11708 RMSE: 92.3636
optimal_tree <- rpart(
  formula = count ~ .,
  data = training.df,
  method = "anova",
  control = list(minsplit = 19, maxdepth = 30, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = validation.df)
accuracy(pred, validation.df$count)




# -- Predict from competition data. Remove negatives and write to CSV ------------------------------------

## Predictions with test/competition data.
pred_test <- predict(optimal_tree, newdata = biketest.df, na.action = na.pass)

# convert negative values to 0
pred_test[pred_test < 0] <- 0

# write submission in kaggle format
# datetime,count
# 2011-01-20 00:00:00,0
write.csv(data.frame(datetime = biketest.df$datetime, count = pred_test),
          file = "output/regression_tree_optimal.csv", row.names=FALSE)



# -- Bagging ???? --------------------------------------
# http://uc-r.github.io/regression_trees#bag




# +++ See Chapter 13 for further details on bagging. --------------------


# -------------- Plots -----------------------------------



# Leadoff Graphic: Histogram of count ----------------------
ggplot() + geom_histogram(data = bikeplot.df, aes(x = count), color = "black",
                                      fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(x = "count (hourly usage count)", y = "Frequency", title = "Histogram of hourly count (usage)") +
  geom_vline(aes(xintercept = median(bikeplot.df$count), colour = "median")) + 
  geom_vline(aes(xintercept = mean(bikeplot.df$count), colour = "mean"))






# hour & dayofweek ----------------------------------

# Bar chart : count by dayofweek ---------
days.of.week <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
dayofweek.bar.data <- group_by(bikeplot.df, dayofweek)
dayofweek.bar.data <- summarise(dayofweek.bar.data, mean_count = mean(count), median_count = median(count))

dayofweek.bar.data <- melt(dayofweek.bar.data, id.vars = 'dayofweek')
dayofweek.bar <- ggplot() + 
  geom_bar(data = dayofweek.bar.data,
           aes(x = dayofweek, y = value, fill = variable),
           stat = 'identity', position = 'dodge', alpha = 0.7) + 
  scale_x_discrete(labels = days.of.week) +
  theme(legend.position="top")
  # coord_flip()
dayofweek.bar


# hour trend line data 
median.hourly.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$hour), FUN = median)
names(median.hourly.count) <- c("hour", "mediancount")


# Tile plot : Count: hour x day -------------------
hour.tile.data <- group_by(bikeplot.df, hour, dayofweek)
hour.tile.data <- summarise(hour.tile.data, mean_count = mean(count))

hour.day.tile <- ggplot(hour.tile.data, aes(hour, dayofweek)) +
  geom_tile(aes(fill = mean_count)) +
  scale_fill_gradient(low = "white", high = "black") +
  scale_y_discrete(labels = days.of.week, breaks = c(7, 6, 5, 4, 3, 2, 1)) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  theme_classic() +
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
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "Count by Hour (color: workingday) w/ median trendline", 
       x = "Hour of Day (00-23)", y = "Count", color = "workingday") +
  # line plot : median.hourly.count
  geom_line(data = median.hourly.count, aes(x = hour, y = mediancount), size = 1, color = "grey25") # +
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
# <<<<<<< VERY GOOD COLORS. Plan vs Log shows heavy use at 31º <<<<<<<<<<<<<<<<<<<<<<<<
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius

# make trend line data first
median.temp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$temp), FUN = median)
median.atemp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$atemp), FUN = median)
names(median.temp.count) <- c("temp", "mediancount")
names(median.atemp.count) <- c("atemp", "mediancount")

# Scatter Plots: Count by Temperature with median usage trendline
# atemp
atemp.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$atemp, 2), y = count),
             pch = 16, colour = "orange", alpha = 0.3) +
  scale_y_sqrt() +
  # geom_line(data = median.atemp.count, aes(x = atemp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.atemp.count, 
              aes(x = atemp, y = mediancount, color = mediancount), 
              method = "auto") +
  # se = FALSE, na.rm = TRUE, formula = y ~ poly(x, 2), method = "lm", 
  labs(x = "atemp", y = "count", title = "count by atemp 'feels like' ºC - w/ median trendline")
atemp.scatter

# temp
temp.scatter <- ggplot() + geom_point(data = bikeplot.df, aes(x = jitter(bikeplot.df$temp, 2), y = count),
                                  pch = 16, colour = "salmon", alpha = 0.3) +
  labs(x = "temp", y = "Count", title = "Count by temp (ºC) - w/ median trendline") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) +
  scale_y_sqrt() +
  # geom_line(data = median.temp.count, aes(x = temp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.temp.count, aes(x = temp, y = mediancount))
  
# Median temp by hour of day: Line Plot
data.for.plot <- aggregate(bikeplot.df$temp, by = list(bikeplot.df$hour), FUN = median)
names(data.for.plot) <- c("hour", "mediantemp")
temp.line <- ggplot() + geom_line(data = data.for.plot, aes(x = hour, y = mediantemp),
                                  stat = "identity") + ylim(0, 41)
  labs(title = "temp varies little through day (~ 5ºC)", y = "median temp", x = "Hour of Day (00-23)")


# Place on grid.
grid.arrange(atemp.scatter, temp.scatter, temp.line, ncol = 3)


# --------- Humidity ----------------------------
# Count by Humidity : JITTERy Scatter Plot


# make trend line data first
median.humidity.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$humidity), FUN = median)
names(median.humidity.count) <- c("humidity", "mediancount")

# - weather: (categorical)
# - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

weather.colors <- c("blue", "orange", "red", "black") # color per level of weather

humidity.weather.scatter <- ggplot() +
  # scatter plot
  geom_point(data = bikeplot.df, aes(x = jitter(humidity, 2), y = count, colour = bikeplot.df$weather), 
             pch = 16, alpha = 0.3) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_y_sqrt() +
  scale_color_manual(values=weather.colors) + 
  labs(title = "Count by Humidity (color: weather) w/ median trendline", 
       x = "Humidity", y = "Count", color = "weather") +
  # line plot : median.humidity.count
  # geom_line(data = median.humidity.count, aes(x = humidity, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.humidity.count, aes(x = humidity, y = mediancount), color = "grey28")
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
  geom_line(data = data.for.plot, aes(x = hour, y = medianhumidity), stat = "identity") +
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
  labs(x = "1 = Holiday, 0 = Not", y = "Count", title = "Count by Holiday/Not Holiday")



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
histo <- ggplot(bikeplot.df) + geom_histogram(aes(x = windspeed), color = "black",
                                               fill = "green4", alpha = 0.3, binwidth = 1) +
  labs(x = "windspeed (unknown units)", y = "Frequency", title = "Histogram: windspeed")

# Place on grid.
grid.arrange(histo, windspeed.scatter, ncol=2)
rm(histo)


# ---- CONGRESS IN SESSION... LOWER DEMAND ??? -------------
## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 2))
boxplot(bikeplot.df$count ~ bikeplot.df$senate, xlab = "senate", ylab = "count", 
        col = "pink", main = "Senate in session?")
boxplot(bikeplot.df$count ~ bikeplot.df$house, xlab = "house", ylab = "count",
        col = "lightblue", main = "House in session?")

# --------- GAME ON? ... INCREASED DEMAND !!!!! -------------
## side-by-side boxplots
measure.vars = c("capitals", "nationals", "united", "wizards", "sporting_event")
keeps <- c(measure.vars, "count")
sports.df <- bikeplot.df[keeps]

# tried to get: variable, value, count (as in...    nationals, 1, 250)
# but I got... count, variable(event), value (0-1)
sports.df <- melt(sports.df, measure.vars = measure.vars) # , varnames = c("event", "value"))

ggplot(sports.df, aes(x = variable, y = count, fill = value)) + geom_boxplot()


sportinghours.train.df <- subset(bikeplot.df, (hour > 17) | (hour > 11 & hour < 16))
ggplot(sportinghours.train.df, 
       aes(x = sporting_event, y = count, fill = sporting_event)) + 
  geom_boxplot() +
  labs(title = "sporting_event: comparable times", subtitle = "(noon - 15:59 or after 17:59)")


# --------- UNIVERSITIES IN SESSION... LOWER DEMAND ??? --------- 
## side-by-side boxplots
par(mfcol = c(1, 3))
boxplot(bikeplot.df$count ~ bikeplot.df$cua_session, xlab = "cua_session", ylab = "count",
        col = "cyan", main = "Catholic U of America")
boxplot(bikeplot.df$count ~ bikeplot.df$au_session, xlab = "au_session", ylab = "count",
        col = "yellow", main = "American University")
boxplot(bikeplot.df$count ~ bikeplot.df$howard_session, xlab = "howard_session", ylab = "count",
        col = "magenta", main = "Howard University")

par(mfcol = c(1, 3))
boxplot(bikeplot.df$count ~ bikeplot.df$session_count, xlab = "session_count", ylab = "count",
        col = "brown", main = "# Unis in Session")

boxplot(bikeplot.df$count ~ bikeplot.df$session_any, xlab = "session_any", ylab = "count",
        col = "dodgerblue3", main = "Any Uni in Session")

# boxplot(bikeplot.df$count ~ bikeplot.df$session_any, xlab = "session_any", ylab = "count",
#         col = "grey", main = "Any University")
hist(as.numeric(bikeplot.df$session_count), xlab = "session_count", 
     col = "slateblue", main = "Histogram of session_count") # , breaks = 3)

par(mfcol = c(1, 1))


seasons <- c("Spring", "Summer", "Fall", "Winter")

session_any.box.data <- bikeplot.df[c('season', 'session_any', 'count')]
# session_any.box.data <- group_by(session_any.box.data, season)
# session_any.box.data <- melt(session_any.box.data, id.vars = 'season')

session_any.box <- ggplot(session_any.box.data, aes(x = season, y = count, fill = session_any)) + 
  geom_boxplot() +
  scale_x_discrete(labels = seasons) +
  theme(legend.position = "top") +
  theme_minimal() +
  labs(title = "session_any is not a proxy for season")




# --------- WEATHER AND SEASON --------- 
## side-by-side boxplots
# par(mfcol = c(1, 2))
# boxplot(bikeplot.df$count ~ bikeplot.df$weather, xlab = "weather", ylab = "count", main = "count by weather",
#         sub = "4 = Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog")
# boxplot(bikeplot.df$count ~ bikeplot.df$season, xlab = "season", ylab = "count", main = "count by season")
weather.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = weather, y = count), pch = 20, color = "black", fill = "blue", alpha = 0.4) + 
  labs(x = "weather (1-4)", y = "Count", title = "Count by weather",
       sub = "4 = Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog")

season.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = season, y = count), pch = 20, color = "black", fill = "green", alpha = 0.4) + 
  labs(x = "season (1 = Spr, 2 = Sum, 3 = Fall, 4 = Win)", y = "Count", title = "Count by season")


# Place on grid.
grid.arrange(weather.box, season.box, ncol=2)
rm(season.box)
rm(weather.box)
