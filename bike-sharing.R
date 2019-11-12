setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)

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


# <<<<<<<<<<<<<< LEADOFF GRAPHIC <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Histogram of count
ggplot(biketrain.df) + geom_histogram(aes(x = count), color = "black",
                                      fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  labs(x = "count (hourly usage count)", y = "Frequency", title = "Histogram of hourly count (usage)")


# <<<<<<< VERY GOOD COLORS. USE THIS. <<<<<<<<<<<<<<<<<<<<<<<<
# Demand by Hour (colored by dayofweek) : JITTERy Scatter Plot
dayofweek.colors <- c("red", "violet", "purple", "blue", "green", "yellow", "orange") # color per day
hour.scatter1 <- ggplot(biketrain.df) + geom_point(aes(x = jitter(hour, 2), y = count, colour = biketrain.df$dayofweek), 
                                                   pch = 20, alpha = 0.5) + 
  scale_color_manual(values=dayofweek.colors) + 
  labs(title = "Demand by Hour (colored by dayofweek)", x = "Hour of Day (00-23)", y = "Count", color = "dayofweek")

workingday.colors <- c("purple", "gray") # color per workday/not
hour.scatter2 <- ggplot(biketrain.df) + geom_point(aes(x = jitter(hour, 2), y = count, colour = biketrain.df$workingday), 
                                                   pch = 20, alpha = 0.5) + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "Demand by Hour (colored by workingday)", x = "Hour of Day (00-23)", y = "Count", color = "workingday")

# Demand by Hour (Mean) : Bar chart
data.for.plot <- aggregate(biketrain.df$count, by = list(biketrain.df$hour), FUN = mean)
names(data.for.plot) <- c("hour", "Meancount")
hour.bar <- ggplot(data.for.plot) + geom_bar(aes(x = hour, y = Meancount), colour = "black", fill = "blue", alpha = 0.5, stat = "identity") + 
  labs(title = "Average Demand by Hour", x = "Hour of Day (00-23)", y = "Mean hourly count")
hour.bar
# Place on grid.
grid.arrange(hour.scatter1, hour.scatter2, hour.bar, nrow=3)



# alternative plot with ggplot

library(reshape) # to generate input for the plot
cor.mat <- round(cor(biketrain.df),2) # rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))





# <<<<<<< VERY GOOD COLORS. USE THIS. <<<<<<<<<<<<<<<<<<<<<<<<
# Demand by Day of Week (with Holidays in red) : JITTERy Scatter Plot
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




# <<<<<<< VERY GOOD COLORS. Plan vs Log shows heavy use at 31º <<<<<<<<<<<<<<<<<<<<<<<<
# Demand by Temperature : JITTERy Scatter Plot
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius
ggplot(biketrain.df) + geom_point(aes(x = jitter(biketrain.df$atemp, 2), y = count),
                                  pch = 20, colour = "orange", alpha = 0.3) +
  labs(x = "Temp (ºC)", y = "Count");
# Log scaling on Y
ggplot(biketrain.df) + geom_point(aes(x = jitter(biketrain.df$atemp, 2), y = count),
                                  pch = 20, colour = "orange", alpha = 0.3) +
  scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # was (breaks = 10^(-2:2), 
  # labels = format(10^(-2:2), scientific = FALSE, drop0trailing = TRUE)) +
  labs(x = "Temp (ºC)", y = "Count");




# <<<<<<< SPRING USE VERY LOW. WAS THIS FIRST SEASON OFFERED? <<<<<<<<<<<<<<<<<<<<<<<<
# Demand by Season : Bar chart
data.for.plot <- aggregate(biketrain.df$count, by = list(biketrain.df$season), FUN = sum)
names(data.for.plot) <- c("season", "Sumcount")
ggplot(data.for.plot) + geom_bar(aes(x = season, y = Sumcount), 
                                 colour = "black", fill = "olivedrab", alpha = 0.7, 
                                 stat = "identity") + 
  labs(x = "Season 1 = spring, 2 = summer, 3 = fall, 4 = winter", y = "Sum of seasonal counts")




# <<<<<<< HOLIDAY USE TRENDS HIGH. NOTE NON-HOLIDAY CLUSERTED VALUES <<<<<<<<<<<<<<<<<<<<<<<<
# Demand : Holiday vs. Not : JITTERy Scatter Plot
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



# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# Demand by Wind Speed : Scatter Plot
# only 30 distinct values, makes this goofy.
scatter <- ggplot(biketrain.df) + geom_point(aes(x = jitter(biketrain.df$windspeed, 6), y = count),
                                             pch = 20, colour = "green4", alpha = 0.3) + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + # Good with our without.
  # scale_x_log10(breaks = c(2, 4, 8, 16, 32, 64)) + 
  labs(x = "Wind Speed (units not provided)", y = "Count")
# Histogram of wind speed
histo <- ggplot(biketrain.df) + geom_histogram(aes(x = windspeed), color = "black",
                                               fill = "green4",alpha = 0.3, binwidth = 1) +
  labs(x = "wind speed (unknown units)", y = "Frequency")
# Place on grid.
grid.arrange(scatter, histo, nrow=2)


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
