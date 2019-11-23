setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(pastecs)
library(reshape2)
library(forecast)

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

biketrain.df <- subset(bikeall.df, train == 1)
biketest.df <- subset(bikeall.df, train == 0)

train.rows   <- rownames(biketrain.df)
test.rows    <- rownames(biketest.df)

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


# Regression model. Not working as of 2019-11-10
# reg <- lm(count ~., data = bikeall.df, subset = train.rows)
# tr.res <- data.frame(biketrain.df$count, reg$fitted.values, reg$residuals)
# head(tr.res)
# 
# ####  From textbook: Table 2.11
# reg <- lm(TOTAL_VALUE ~ .-TAX, data = housing.df, subset = train.rows) # remove variable "TAX"
# tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)
# head(tr.res)

# -------------- Plots -----------------------------------



# Leadoff Graphic: Histogram of count ----------------------
ggplot() + geom_histogram(data = biketrain.df, aes(x = count), color = "black",
                                      fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(x = "count (hourly usage count)", y = "Frequency", title = "Histogram of hourly count (usage)") +
  geom_vline(aes(xintercept = median(biketrain.df$count), colour = "median")) + 
  geom_vline(aes(xintercept = mean(biketrain.df$count), colour = "mean"))



# hour & dayofweek ----------------------------------

# Bar chart : count by dayofweek ---------
days.of.week <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
by.dayofweek <- group_by(biketrain.df, dayofweek)
dayofweek.bar.data <- summarise(by.dayofweek, mean_count = mean(count), median_count = median(count))

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
median.hourly.count <- aggregate(biketrain.df$count, by = list(biketrain.df$hour), FUN = median)
names(median.hourly.count) <- c("hour", "mediancount")


# Tile plot : Count: hour x day -------------------
by.hour <- group_by(biketrain.df, hour, dayofweek)
tile.data <- summarise(by.hour, mean_count = mean(count))

hour.day.tile <- ggplot(tile.data, aes(hour, dayofweek)) +
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
#   geom_point(data = biketrain.df, aes(x = jitter(hour, 2), y = count, colour = biketrain.df$dayofweek), 
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
  geom_point(data = biketrain.df, aes(x = jitter(hour, 2), y = count, colour = biketrain.df$workingday), 
             pch = 16, alpha = 0.3) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "Count by Hour (color: workingday) w/ median trendline", 
       x = "Hour of Day (00-23)", y = "Count", color = "workingday") +
  # line plot : median.hourly.count
  geom_line(data = median.hourly.count, aes(x = hour, y = mediancount), size = 1, color = "grey25") +
  geom_smooth(data = median.hourly.count, aes(x = hour, y = mediancount), formula = y ~ x, method = "lm", color = "navy", se = FALSE, na.rm = TRUE)


# Place on grid.
grid.arrange(dayofweek.bar, hour.day.tile, hour.scatter2, ncol = 3) # , widths = c(1, 2, 2))




# dayofweek --------------------------- 
# JITTERy Scatter Plot: count by dayofweek (Holidays in red) 
holiday.colors <- c("0" = "blue", "1" = "red")
ggplot(biketrain.df) + geom_point(aes(x = jitter(as.numeric(dayofweek), 2), y = count, colour = biketrain.df$holiday), 
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
median.temp.count <- aggregate(biketrain.df$count, by = list(biketrain.df$temp), FUN = median)
median.atemp.count <- aggregate(biketrain.df$count, by = list(biketrain.df$atemp), FUN = median)
names(median.temp.count) <- c("temp", "mediancount")
names(median.atemp.count) <- c("atemp", "mediancount")

# Scatter Plots: Count by Temperature with median usage trendline
# atemp
atemp.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$atemp, 2), y = count),
                                  pch = 16, colour = "orange", alpha = 0.3) +
  labs(x = "atemp", y = "Count", title = "Count by 'Feels Like Temp' (ºC) - w/ median trendline") + 
  scale_y_sqrt() +
  geom_line(data = median.atemp.count, aes(x = atemp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.atemp.count, aes(x = atemp, y = mediancount), formula = y ~ poly(x, 2), method = "lm", color = "navy", se = FALSE, na.rm = TRUE)

# temp
temp.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$temp, 2), y = count),
                                  pch = 16, colour = "salmon", alpha = 0.3) +
  labs(x = "temp", y = "Count", title = "Count by temp (ºC) - w/ median trendline") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) +
  scale_y_sqrt() +
  geom_line(data = median.temp.count, aes(x = temp, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.temp.count, aes(x = temp, y = mediancount), formula = y ~ poly(x, 2), method = "lm", color = "navy", se = FALSE, na.rm = TRUE)
  
# Median temp by hour of day: Line Plot
data.for.plot <- aggregate(biketrain.df$temp, by = list(biketrain.df$hour), FUN = median)
names(data.for.plot) <- c("hour", "mediantemp")
temp.line <- ggplot() + geom_line(data = data.for.plot, aes(x = hour, y = mediantemp),
                                  stat = "identity") + ylim(0, 41)
  labs(title = "temp varies little through day (~ 5ºC)", y = "median temp", x = "Hour of Day (00-23)")


geom_smooth(formula = y ~ poly(x, 2), method= "lm",
            colour = "navy", se = FALSE, na.rm = TRUE)


# Place on grid.
grid.arrange(atemp.scatter, temp.scatter, temp.line, ncol = 3)


# --------- Humidity ----------------------------
# Count by Humidity : JITTERy Scatter Plot


# make trend line data first
median.humidity.count <- aggregate(biketrain.df$count, by = list(biketrain.df$humidity), FUN = median)
names(median.humidity.count) <- c("humidity", "mediancount")

# - weather: (categorical)
# - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

weather.colors <- c("blue", "orange", "red", "black") # color per level of weather

humidity.weather.scatter <- ggplot() +
  # scatter plot
  geom_point(data = biketrain.df, aes(x = jitter(humidity, 2), y = count, colour = biketrain.df$weather), 
             pch = 16, alpha = 0.3) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_y_sqrt() +
  scale_color_manual(values=weather.colors) + 
  labs(title = "Count by Humidity (color: weather) w/ median trendline", 
       x = "Humidity", y = "Count", color = "weather") +
  # line plot : median.humidity.count
  geom_line(data = median.humidity.count, aes(x = humidity, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.humidity.count, aes(x = humidity, y = mediancount), formula = y ~ poly(x, 2), method = "lm", color = "navy", se = FALSE, na.rm = TRUE)
humidity.weather.scatter


# Humidity by hour Scatterplot w/ median trendline
data.for.plot <- aggregate(biketrain.df$humidity, by = list(biketrain.df$hour), FUN = median)
names(data.for.plot) <- c("hour", "medianhumidity")
humidity.by.hour <- ggplot() + 
  # geom_bar(data = median.hourly.count, aes(x = hour, y = mediancount / 5),
  #          colour = "grey", fill = "grey", alpha = 0.2, 
  #          stat = "identity") +
  geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$hour, 2), y = humidity),
                                  pch = 20, colour = "seagreen3", alpha = 0.3) +
  geom_line(data = data.for.plot, aes(x = hour, y = medianhumidity), stat = "identity") +
  labs(x = "Hour of Day (00-23)", y = "humidity",
       title = "Humidity by hour w/ median trendline",
       color = "medianhumidity")
humidity.by.hour

# Scatter Plot : atemp vs humidity
# atemp.humid.scatter <- ggplot() +
#   geom_point(data = biketrain.df, aes(x = jitter(humidity, 2), y = atemp),
#              pch = 20, color = "violetred", alpha = 0.3) +
#   labs(x = "humidity", y = "atemp", title = "atemp vs humidity")
# atemp.humid.scatter

# Place on grid.
grid.arrange(humidity.weather.scatter, humidity.by.hour, ncol = 2)


# Count : Holiday vs. Not : JITTERy Scatter Plot
# -------  HOLIDAY USE BOX PLOT --------------
# As a boxplot
ggplot(biketrain.df) + geom_boxplot(aes(x = holiday, y = count), 
                                    pch = 20, color = "brown", fill = "rosybrown2", alpha = 0.4) + 
  labs(x = "1 = Holiday, 0 = Not", y = "Count", title = "Count by Holiday/Not Holiday")



# ---- windspeed ----------------------------------
# Count by Wind Speed : Scatter Plot
# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# only 30 distinct values, makes this goofy.

median.windspeed.count <- aggregate(biketrain.df$count, by = list(biketrain.df$windspeed), FUN = median)
names(median.windspeed.count) <- c("windspeed", "mediancount")

# Count by Hour (median) : Bar chart
windspeed.bar <- ggplot(median.windspeed.count) + geom_bar(aes(x = windspeed, y = mediancount), 
                                               colour = "black", fill = "blue", 
                                               alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "median count by windspeed", x = "windspeed", y = "median count")
windspeed.bar


windspeed.scatter <- ggplot() + 
  geom_point(data = biketrain.df, 
             aes(x = jitter(biketrain.df$windspeed, 6), y = count),
             pch = 20,
             # colour = biketrain.df$season,
             # colour = biketrain.df$hour %/% 5 + 1,
             colour = "green4",
             alpha = 0.3) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # Good with our without.
  # scale_x_log10(breaks = c(2, 4, 8, 16, 32, 64)) + 
  scale_y_sqrt() +
  # scale_color_manual(values=weather.colors) + 
  labs(x = "Wind Speed (units not provided)", y = "Count", title = "Count by Wind Speed - with median count trendline") +
  geom_line(data = median.windspeed.count, aes(x = windspeed, y = mediancount), size = 1, color = "grey28") +
  geom_smooth(data = median.windspeed.count, aes(x = windspeed, y = mediancount), formula = y ~ poly(x, 2), method = "lm", color = "navy", se = FALSE, na.rm = TRUE)
  

# Histogram of windspeed speed
histo <- ggplot(biketrain.df) + geom_histogram(aes(x = windspeed), color = "black",
                                               fill = "green4", alpha = 0.3, binwidth = 1) +
  labs(x = "windspeed (unknown units)", y = "Frequency", title = "Histogram: windspeed")

# Place on grid.
grid.arrange(histo, windspeed.scatter, ncol=2)


# ---- CONGRESS IN SESSION... LOWER DEMAND ??? -------------
## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 2))
boxplot(biketrain.df$count ~ biketrain.df$senate, xlab = "senate", ylab = "count", 
        col = "pink", main = "Senate in session?")
boxplot(biketrain.df$count ~ biketrain.df$house, xlab = "house", ylab = "count",
        col = "lightblue", main = "House in session?")

# --------- GAME ON? ... INCREASED DEMAND !!!!! -------------
## side-by-side boxplots
par(mfcol = c(1, 5))
boxplot(biketrain.df$count ~ biketrain.df$capitals, xlab = "capitals",
        ylab = "count", frame = F, col = "indianred")
boxplot(biketrain.df$count ~ biketrain.df$nationals, xlab = "nationals", 
        ylab =  NA, yaxt = "n", frame = F, col = "mediumpurple")
boxplot(biketrain.df$count ~ biketrain.df$wizards, xlab = "wizards", 
        ylab =  NA, yaxt = "n", frame = F, col = "lavenderblush")
boxplot(biketrain.df$count ~ biketrain.df$united, xlab = "united",  
        ylab =  NA, yaxt = "n", frame = F, col = "slateblue")
boxplot(biketrain.df$count ~ biketrain.df$sporting_event, xlab = "sporting_event",
        ylab =  NA, yaxt = "n", frame = F, col = "darkred")

par(mfcol = c(1, 1))
sportinghours.train.df <- subset(biketrain.df, (hour > 17) | (hour > 11 & hour < 16))
wknd.sportinghours.train.df <- subset(sportinghours.train.df, workingday == 0)
boxplot(wknd.sportinghours.train.df$count ~ wknd.sportinghours.train.df$sporting_event, 
        xlab = "sporting_event", ylab = "count", frame = F,
        col = "darkred", main = "sporting_event: comparable times", sub = "(noon - 15:59 or after 17:59)")


# --------- UNIVERSITIES IN SESSION... LOWER DEMAND ??? <<<<<<<<<<<<<<<<<<<<<
## side-by-side boxplots
par(mfcol = c(1, 3))
boxplot(biketrain.df$count ~ biketrain.df$cua_session, xlab = "cua_session", ylab = "count",
        col = "cyan", main = "Catholic U of America")
boxplot(biketrain.df$count ~ biketrain.df$au_session, xlab = "au_session", ylab = "count",
        col = "yellow", main = "American University")
boxplot(biketrain.df$count ~ biketrain.df$howard_session, xlab = "howard_session", ylab = "count",
        col = "magenta", main = "Howard University")

par(mfcol = c(1, 3))
boxplot(biketrain.df$count ~ biketrain.df$session_count, xlab = "session_count", ylab = "count",
        col = "brown", main = "# Unis in Session")
boxplot(biketrain.df$count ~ biketrain.df$session_any, xlab = "session_any", ylab = "count",
        col = "grey", main = "Any University")
hist(as.numeric(biketrain.df$session_count), xlab = "session_count", 
     col = "slateblue", main = "Histogram of session_count") # , breaks = 3)

par(mfcol = c(1, 1))

# --------- WEATHER AND SEASON <<<<<<<<<<<<<<<<<<<<<
## side-by-side boxplots
# par(mfcol = c(1, 2))
# boxplot(biketrain.df$count ~ biketrain.df$weather, xlab = "weather", ylab = "count", main = "count by weather",
#         sub = "4 = Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog")
# boxplot(biketrain.df$count ~ biketrain.df$season, xlab = "season", ylab = "count", main = "count by season")
weather.box <- ggplot(biketrain.df) + 
  geom_boxplot(aes(x = weather, y = count), pch = 20, color = "black", fill = "blue", alpha = 0.4) + 
  labs(x = "weather (1-4)", y = "Count", title = "Count by weather",
       sub = "4 = Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog")

season.box <- ggplot(biketrain.df) + 
  geom_boxplot(aes(x = season, y = count), pch = 20, color = "black", fill = "green", alpha = 0.4) + 
  labs(x = "season (1 = Spr, 2 = Sum, 3 = Fall, 4 = Win)", y = "Count", title = "Count by season")


# Place on grid.
grid.arrange(weather.box, season.box, ncol=2)
