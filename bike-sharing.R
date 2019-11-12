setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(pastecs)
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
bikeall.df[,'nationals']<-factor(bikeall.df[,'nationals'])
bikeall.df[,'united']<-factor(bikeall.df[,'united'])
bikeall.df[,'wizards']<-factor(bikeall.df[,'wizards'])
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
View(bikeall.df)

stat.desc(bikeall.df)

summary(bikeall.df)
mean(bikeall.dff$count)
bikeall.df[,c("datetime","count","nationals")]

str(bikeall.df)
describe(bikeall.df)
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

# Histogram of count
# <<<<<<<<<<<<<< LEADOFF GRAPHIC <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ggplot(biketrain.df) + geom_histogram(aes(x = count), color = "black",
                                      fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(x = "count (hourly usage count)", y = "Frequency", title = "Histogram of hourly count (usage)")



# Demand by Hour (colored by dayofweek) : JITTERy Scatter Plot
# <<<<<<< VERY GOOD COLORS. USE THIS. <<<<<<<<<<<<<<<<<<<<<<<<
# make trend line data first
mean.hourly.count <- aggregate(biketrain.df$count, by = list(biketrain.df$hour), FUN = mean)
names(mean.hourly.count) <- c("hour", "Meancount")

# Demand by Hour (Mean) : Bar chart
hour.bar <- ggplot(mean.hourly.count) + geom_bar(aes(x = hour, y = Meancount), 
                                             colour = "black", fill = "blue", 
                                             alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "Average Demand by Hour", x = "Hour of Day (00-23)", y = "Mean hourly count")
hour.bar

# JITTERy Scatter Plot : (colored by dayofweek)
dayofweek.colors <- c("red", "violet", "purple", "blue", "green", "yellow", "orange") # color per day

hour.scatter1 <- ggplot() +
  # scatter plot
  geom_point(data = biketrain.df, aes(x = jitter(hour, 2), y = count, colour = biketrain.df$dayofweek), 
             pch = 20, alpha = 0.5) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_color_manual(values=dayofweek.colors) + 
  labs(title = "Demand by Hour (colored by dayofweek) with mean count trendline", 
       x = "Hour of Day (00-23)", y = "Count", color = "dayofweek") +
  # line plot : median.hourly.count
  geom_line(data = mean.hourly.count, aes(x = hour, y = Meancount), size = 1)

# hour.scatter1

# JITTERy Scatter Plot : (colored by workday/not)
workingday.colors <- c("purple", "gray") # color per workday/not
hour.scatter2 <- ggplot() +
  # scatter plot
  geom_point(data = biketrain.df, aes(x = jitter(hour, 2), y = count, colour = biketrain.df$workingday), 
             pch = 20, alpha = 0.5) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "Demand by Hour (colored by workingday) with mean count trendline", 
       x = "Hour of Day (00-23)", y = "Count", color = "workingday") +
  # line plot : median.hourly.count
  geom_line(data = mean.hourly.count, aes(x = hour, y = Meancount), size = 1) +
  labs(title = "Hourly mean count trendline")

# Place on grid.
grid.arrange(hour.scatter1, hour.scatter2, ncol=2)



# alternative plot with ggplot

library(reshape) # to generate input for the plot
cor.mat <- round(cor(biketrain.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))





# Demand by Day of Week (with Holidays in red) : JITTERy Scatter Plot
# <<<<<<< VERY GOOD COLORS. USE THIS. <<<<<<<<<<<<<<<<<<<<<<<<
holiday.colors <- c("0" = "blue", "1" = "red")
ggplot(biketrain.df) + geom_point(aes(x = jitter(as.numeric(dayofweek), 2), y = count, colour = biketrain.df$holiday), 
                                  pch = 20, alpha = 0.3) +
  scale_color_manual(values = holiday.colors) +
  labs(title = "Demand by Day of Week (with Holidays in red)", 
       x = "Day of Week (1-7 | Sun - Sat)", 
       y = "Count", 
       color = "Holiday?")

# Demand by Day of Week : Bar chart
data.for.plot <- aggregate(biketrain.df$count, by = list(biketrain.df$dayofweek), FUN = sum)
names(data.for.plot) <- c("dayofweek", "Sumcount")
ggplot(data.for.plot) + geom_bar(aes(x = dayofweek, y = Sumcount), 
                                 color = "black", fill = "chocolate", stat = "identity") + 
  labs(x = "Day of Week (1-7, Sun - Sat)", y = "Sum of weekly counts")




# -- From book. Meh so far ---
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal, 
# and scatter plots in the remaining panels.
# plot(biketrain.df[, c(3, 4, 25, 15)])
# # alternative, nicer plot
# library(GGally)
# ggpairs(biketrain.df[, c("hour", "dayofweek", "session_any", "count")])



# Demand by Temperature : JITTERy Scatter Plot
# ------- Usage rarely low when temp is high -----------------------
# <<<<<<< VERY GOOD COLORS. Plan vs Log shows heavy use at 31º <<<<<<<<<<<<<<<<<<<<<<<<
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius

# make trend line data first
mean.temp.count <- aggregate(biketrain.df$count, by = list(biketrain.df$temp), FUN = mean)
names(mean.temp.count) <- c("temp", "Meancount")

# Demand by Hour (Mean) : Bar chart
temp.bar <- ggplot(mean.temp.count) + geom_bar(aes(x = temp, y = Meancount), 
                                                 colour = "black", fill = "blue", 
                                                 alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "Mean count by temp", x = "temp", y = "Mean count")
temp.bar


atemp.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$atemp, 2), y = count),
                                  pch = 20, colour = "orange", alpha = 0.3) +
  labs(x = "atemp", y = "Count", title = "Usage count by 'Feels Like Temp' (ºC) - Scatterplot - with mean count trendline") + 
  geom_line(data = mean.temp.count, aes(x = temp, y = Meancount), size = 1)


temp.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$temp, 2), y = count),
                                  pch = 20, colour = "salmon", alpha = 0.3) +
  labs(x = "temp", y = "Count", title = "Usage count by temp (ºC) - Scatterplot - with mean count trendline") + 
  geom_line(data = mean.temp.count, aes(x = temp, y = Meancount), size = 1)

# Log scaling on Y
logatemp.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$atemp, 2), y = count),
                                  pch = 20, colour = "orange", alpha = 0.3) +
  scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) +
  labs(x = "atemp", y = "Count", title = "Usage count by 'Feels Like Temp'  (ºC) : scale_y_log10 - Scatterplot - with mean count trendline") + 
  geom_line(data = mean.temp.count, aes(x = temp, y = Meancount), size = 1)


ggplot(biketrain.df) + geom_point(aes(x = jitter(biketrain.df$hour, 2), y = temp),
                                  pch = 20, colour = "salmon4", alpha = 0.3) +
  labs(x = "Hour", y = "Temp", title = "Temp (ºC) by Hour of day - Scatterplot");

# Mean temp by hour of day
data.for.plot <- aggregate(biketrain.df$temp, by = list(biketrain.df$hour), FUN = mean)
names(data.for.plot) <- c("hour", "Meantemp")
temp.line <- ggplot() + geom_line(data = data.for.plot, aes(x = hour, y = Meantemp),
                                  stat = "identity") + ylim(0, 41) +
  labs(title = "temp varies little through day (~ 5ºC)", y = "mean temp", x = "Hour of Day (00-23)")

# Place on grid.
grid.arrange(atemp.scatter, logatemp.scatter, temp.scatter, temp.line, ncol = 2, nrow = 2)






# Demand by Season : Bar chart
# <<<<<<< SPRING USE VERY LOW. WAS THIS FIRST SEASON OFFERED? <<<<<<<<<<<<<<<<<<<<<<<<
data.for.plot <- aggregate(biketrain.df$count, by = list(biketrain.df$season), FUN = sum)
names(data.for.plot) <- c("season", "Sumcount")
ggplot(data.for.plot) + geom_bar(aes(x = season, y = Sumcount), 
                                 colour = "black", fill = "olivedrab", alpha = 0.7, 
                                 stat = "identity") + 
  labs(x = "Season 1 = spring, 2 = summer, 3 = fall, 4 = winter", y = "Sum of seasonal counts")



# Demand : Holiday vs. Not : JITTERy Scatter Plot
# <<<<<<< HOLIDAY USE TRENDS HIGH. NOTE NON-HOLIDAY CLUSERTED VALUES <<<<<<<<<<<<<<<<<<<<<<<<
ggplot(biketrain.df) + geom_point(aes(x = jitter(as.numeric(biketrain.df$holiday), 3), y = count),
                                  pch = 20, colour = "rosybrown2", alpha = 0.5) + 
  labs(x = "1 = Holiday, 0 = Not", y = "Count")
# Log scaling on Y (Doesn't help.)
ggplot(biketrain.df) + geom_point(aes(x = jitter(as.numeric(biketrain.df$holiday), 3), y = count),
                                  pch = 20, colour = "rosybrown2", alpha = 0.5) + 
  scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) +
  labs(x = "1 = Holiday, 0 = Not", y = "Count")

# As a boxplot
ggplot(biketrain.df) + geom_boxplot(aes(x = holiday, y = count), 
                                    pch = 20, color = "brown", fill = "rosybrown2", alpha = 0.3) + 
  labs(x = "1 = Holiday, 0 = Not", y = "Count")




# Demand by Wind Speed : Scatter Plot
# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# only 30 distinct values, makes this goofy.


mean.windspeed.count <- aggregate(biketrain.df$count, by = list(biketrain.df$windspeed), FUN = mean)
names(mean.windspeed.count) <- c("windspeed", "Meancount")

# Demand by Hour (Mean) : Bar chart
windspeed.bar <- ggplot(mean.windspeed.count) + geom_bar(aes(x = windspeed, y = Meancount), 
                                               colour = "black", fill = "blue", 
                                               alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "Mean count by windspeed", x = "windspeed", y = "Mean count")
windspeed.bar


windspeed.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$windspeed, 6), y = count),
                                      pch = 20, colour = "green4", alpha = 0.3) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # Good with our without.
  # scale_x_log10(breaks = c(2, 4, 8, 16, 32, 64)) + 
  labs(x = "Wind Speed (units not provided)", y = "Count", title = "Count by Wind Speed - with mean count trendline") +
  geom_line(data = mean.windspeed.count, aes(x = windspeed, y = Meancount), size = 1)

logwindspeed.scatter <- ggplot() + geom_point(data = biketrain.df, aes(x = jitter(biketrain.df$windspeed, 6), y = count),
                                      pch = 20, colour = "green4", alpha = 0.3) + 
  scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # Good with our without.
  # scale_x_log10(breaks = c(2, 4, 8, 16, 32, 64)) + 
  labs(x = "Wind Speed (units not provided)", y = "Count", title = "Count by Wind Speed - scale_y_log10 - with mean count trendline") +
  geom_line(data = mean.windspeed.count, aes(x = windspeed, y = Meancount), size = 1)

# Histogram of windspeed speed
histo <- ggplot(biketrain.df) + geom_histogram(aes(x = windspeed), color = "black",
                                               fill = "green4",alpha = 0.3, binwidth = 1) +
  labs(x = "windspeed (unknown units)", y = "Frequency", title = "Histogram: windspeed")
# Place on grid.
grid.arrange(windspeed.scatter, logwindspeed.scatter, histo, ncol=3)


# <<<<<<<< CONGRESS IN SESSION... LOWER DEMAND ??? <<<<<<<<<<<<<<<<<<<<<
## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 2))
boxplot(biketrain.df$count ~ biketrain.df$senate, xlab = "senate", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$house, xlab = "house", ylab = "count")

# <<<<<<<< GAME ON? ... INCREASED DEMAND !!!!! <<<<<<0<<<<<<<<<<<<<<<<<<<<<
## side-by-side boxplots
par(mfcol = c(1, 4))
boxplot(biketrain.df$count ~ biketrain.df$nationals, xlab = "nationals", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$wizards, xlab = "wizards", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$united, xlab = "united", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$sporting_event, xlab = "sporting_event", ylab = "count")

# <<<<<<<< UNIVERSITIES IN SESSION... LOWER DEMAND ??? <<<<<<<<<<<<<<<<<<<<<
## side-by-side boxplots
par(mfcol = c(2, 3))
boxplot(biketrain.df$count ~ biketrain.df$cua_session, xlab = "cua_session", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$session_any, xlab = "session_any", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$howard_session, xlab = "howard_session", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$session_count, xlab = "session_count", ylab = "count")
boxplot(biketrain.df$count ~ biketrain.df$au_session, xlab = "au_session", ylab = "count")
hist(as.numeric(biketrain.df$session_count), xlab = "session_count", main = "Histogram of session_count")

## side-by-side boxplots
par(mfcol = c(1, 2))
boxplot(biketrain.df$count ~ biketrain.df$weather, xlab = "weather", ylab = "count", main = "Histogram: count by weather",
        sub = "4 = Heavy Rain + Ice Pellets + Thunderstorm + Mist, Snow + Fog")
boxplot(biketrain.df$count ~ biketrain.df$season, xlab = "season", ylab = "count", main = "Histogram: count by season")
