# -- Plots ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

# Run `bike-sharing-setup.R` first.
# That's where the library() calls and CSV reads live.


# All the vars for plotting. ----------------

# Using train == 1 because we need `count`
bikeplot.df <- subset(bikeall.df, train == 1)

# Shave off the highest outliers in count
bikeplot.df$count_shaved <- bikeplot.df$count
bikeplot.df$count_shaved[bikeplot.df$count_shaved > 
                           quantile(bikeplot.df$count_shaved, c(.90))] <- quantile(bikeplot.df$count_shaved, c(.90))


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
count.histo <- ggplot() + geom_histogram(data = bikeplot.df, aes(x = count), color = "black",
                          fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  expand_limits(x = 1000, y = 1500) +
  labs(x = "count", y = "Frequency", title = "Histogram of count")
  # geom_vline(aes(xintercept = mean(bikeplot.df$count), colour = "mean")) + 
  # geom_vline(aes(xintercept = mean(bikeplot.df$count), colour = "mean")) +
  # geom_vline(aes(xintercept = quantile(bikeplot.df$count, .90), color = "90%"))

peak.histo <- ggplot() + geom_histogram(data = subset(bikeplot.df, hour > 8 & hour < 21), aes(x = count), color = "black",
                          fill = "darkgoldenrod3",alpha = 0.3, binwidth = 25) +
  expand_limits(x = 1000, y = 1500) +
  labs(y = "Frequency", title = "peak hours")

offpeak.histo <- ggplot() + geom_histogram(data = subset(bikeplot.df, hour < 9 | hour > 20), aes(x = count), color = "black",
                          fill = "royalblue3",alpha = 0.3, binwidth = 25) +
  expand_limits(x = 1000, y = 1500) +
  labs(y = "Frequency", title = "offpeak hours")
  
grid.arrange(count.histo, peak.histo, offpeak.histo, ncol = 3)
  

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
mean.hourly.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$hour), FUN = mean)
names(mean.hourly.count) <- c("hour", "meancount")
# mean.weekday.hourly.count <- subset(bikeplot.df, workingday == 0, select = c("hour", "count"))
# mean.weekday.hourly.count <- aggregate(mean.weekday.hourly.count,
#                                          by = list(mean.weekday.hourly.count$hour), FUN = mean)

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
#   labs(title = "Count by Hour (color: dayofweek) w/ mean trendline", 
#        x = "Hour of Day (00-23)", y = "Count", color = "dayofweek") +
#   # line plot : mean.hourly.count
#   geom_line(data = mean.hourly.count, aes(x = hour, y = meancount), size = 1)

# hour.scatter1

# JITTERy Scatter Plot : (color: workingday)
workingday.colors <- c("blue3", "darkorange2") # color per workday/not
hour.scatter2 <- ggplot() +
  # scatter plot
  geom_point(data = bikeplot.df, aes(x = jitter(hour, 2), y = count, colour = bikeplot.df$workingday), 
             pch = 16, alpha = 0.3) + 
  scale_y_sqrt() + 
  scale_color_manual(values=workingday.colors) + 
  labs(title = "count by hour w/ mean trendline", subtitle = "color: workingday",
       x = "Hour of Day (00-23)", y = "Count", color = "workingday") +
  # line plot : mean.hourly.count
  geom_line(data = mean.hourly.count, aes(x = hour, y = meancount), size = 1, color = "grey25") +
  # geom_line(data = mean.weekday.hourly.count, aes(x = hour, y = count), size = 1) +
  theme(legend.position="top")
# geom_smooth(data = mean.hourly.count, aes(x = hour, y = meancount), method = "auto")


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


# -------  holiday & is_daylight BOX PLOT --------------

ggplot(bikeplot.df) + geom_boxplot(aes(x = holiday, y = count), 
                                   pch = 20, color = "brown", fill = "rosybrown2", alpha = 0.4) + 
  labs(x = "holiday", y = "count", title = "count by holiday (binary)")

measure.vars = c("holiday", "is_daylight")
keeps <- c(measure.vars, "count")
holiday_light.df <- bikeplot.df[keeps]

holiday_light.df <- melt(holiday_light.df, measure.vars = measure.vars)

ggplot(holiday_light.df, aes(x = variable, y = count, fill = value)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "holiday and is_daylight")




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





# Temperature (atemp & temp) ----------------------------
# Count rarely low when temp is high 
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius

# make trend line data first
mean.temp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$temp), FUN = mean)
mean.atemp.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$atemp), FUN = mean)
names(mean.temp.count) <- c("temp", "meancount")
names(mean.atemp.count) <- c("atemp", "meancount")



temp.histo.data <- data.frame(atemp = as.factor(bikeplot.df$atemp), 
                              temp = as.factor(bikeplot.df$temp))
ggplot(data = temp.histo.data, aes(x = temp)) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Scatter Plots: Count by Temperature with mean usage trendline
# atemp
atemp.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$atemp, 2), y = count),
             pch = 16, colour = "orange", alpha = 0.3) + 
  geom_line(data = mean.atemp.count, aes(x = atemp, y = meancount), size = 1, color = "grey28") +
  geom_smooth(data = mean.atemp.count, 
              aes(x = atemp, y = meancount, color = meancount), 
              method = "auto") +
  # se = FALSE, na.rm = TRUE, formula = y ~ poly(x, 2), method = "lm", 
  labs(x = "atemp", y = "count", title = "count by atemp - 'feels like' ºC", 
       subtitle = "mean trendline/loess smoothing")
atemp.scatter


peakplot.df = subset(bikeplot.df, hour > 8 & hour < 21)
offpeakplot.df = subset(bikeplot.df, hour < 9 | hour > 20)

# Peak
mean.peak.atemp.count <- aggregate(peakplot.df$count, by = list(peakplot.df$atemp), FUN = mean)
names(mean.peak.atemp.count) <- c("atemp", "meancount")
atemp.peak.scatter <- ggplot(data = peakplot.df) + 
  geom_point(aes(x = jitter(peakplot.df$atemp, 2), y = count, color = is_daylight),
             pch = 16, alpha = 0.3) +
  geom_line(data = mean.peak.atemp.count, aes(x = atemp, y = meancount), size = 1) +
  geom_smooth(aes(x = atemp, y = count),
              color = "green",
              method = "gam", formula = y ~ s(x)) +
  # theme(legend.position = "top") +
  labs(x = "atemp", y = "count", title = "count vs. atemp @ peak (hour > 8 & hour < 21)", 
       subtitle = "method = 'gam', formula = y ~ s(x))")
atemp.peak.scatter

# Offpeak
mean.offpeak.atemp.count <- aggregate(offpeakplot.df$count, by = list(offpeakplot.df$atemp), FUN = mean)
names(mean.offpeak.atemp.count) <- c("atemp", "meancount")
atemp.offpeak.scatter <- ggplot(data = offpeakplot.df) + 
  geom_point(aes(x = jitter(offpeakplot.df$atemp, 2), y = count, color = is_daylight),
             pch = 16, alpha = 0.3) +
  geom_line(data = mean.offpeak.atemp.count, aes(x = atemp, y = meancount), size = 1) +
  geom_smooth(aes(x = atemp, y = count),
              color = "green",
              method = "gam", formula = y ~ s(x)) +
  expand_limits(y = 1000) +
  theme(legend.position = "top") +
  labs(x = "atemp", y = "count", title = "count vs. atemp @ offpeak (hour < 9 | hour > 20)", 
       subtitle = "method = 'gam', formula = y ~ s(x))")
atemp.offpeak.scatter

grid.arrange(atemp.peak.scatter, atemp.offpeak.scatter, ncol = 2)




# temp
temp.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$temp, 2), y = count),
             pch = 16, colour = "salmon", alpha = 0.3) +
  geom_line(data = mean.temp.count, aes(x = temp, y = meancount), size = 1, color = "grey28") +
  geom_smooth(data = mean.temp.count, aes(x = temp, y = meancount)) +
  labs(x = "temp", y = "ount", title = "Count by temp (ºC) - w/ mean trend")
# stat_smooth(data = bikeplot.df, aes(x = temp, y = count), color = "red", method = gam, formula = y ~ s(x)) +
# stat_smooth(data = bikeplot.df, aes(x = temp, y = count), color = "green", method = lm, formula = y ~ x)

temp.scatter

# mean temp by hour of day: Line Plot
data.for.plot <- aggregate(bikeplot.df$temp, by = list(bikeplot.df$hour), FUN = mean)
names(data.for.plot) <- c("hour", "meantemp")
temp.line <- ggplot() + geom_line(data = data.for.plot, aes(x = hour, y = meantemp),
                                  stat = "identity") + ylim(0, 41) +
  labs(title = "temp varies little through day (~ 5ºC)", y = "mean temp", x = "Hour of Day (00-23)")


# Place on grid.
grid.arrange(atemp.scatter, temp.scatter, temp.line, ncol = 3)






# --------- Humidity ----------------------------
# Count by Humidity : JITTERy Scatter Plot


# make trend line data first
mean.humidity.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$humidity), FUN = mean)
names(mean.humidity.count) <- c("humidity", "meancount")

humidity.weather.scatter <- ggplot() +
  # scatter plot
  geom_point(data = bikeplot.df, aes(x = jitter(humidity, 2), y = count, colour = bikeplot.df$weather), 
             pch = 16, alpha = 0.3) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  scale_y_sqrt() +
  scale_color_manual(values=weather.colors) +
  theme(legend.position="top") + 
  labs(x = "humidity", y = "count", color = "weather", title = "count by humidity | mean trendline/loess smoothing") +
  geom_line(data = mean.humidity.count, aes(x = humidity, y = meancount), size = 1, color = "grey28") +
  geom_smooth(data = mean.humidity.count, aes(x = humidity, y = meancount), color = "blue")
  # stat_smooth(data = bikeplot.df, aes(x = humidity, y = count), color = "red", method = gam, formula = y ~ s(x)) +

humidity.weather.scatter


# Humidity by hour Scatterplot w/ mean trendline
data.for.plot <- aggregate(bikeplot.df$humidity, by = list(bikeplot.df$hour), FUN = mean)
names(data.for.plot) <- c("hour", "meanhumidity")
humidity.by.hour <- ggplot() + 
  # geom_bar(data = mean.hourly.count, aes(x = hour, y = meancount / 5),
  #          colour = "grey", fill = "grey", alpha = 0.2, 
  #          stat = "identity") +
  geom_point(data = bikeplot.df, aes(x = jitter(bikeplot.df$hour, 2), y = humidity),
             pch = 20, colour = "seagreen3", alpha = 0.3) +
  geom_line(data = data.for.plot, aes(x = hour, y = meanhumidity), stat = "identity") +
labs(x = "Hour of Day (00-23)", y = "humidity",
     title = "humidity by hour", subtitle = "mean trendline")
humidity.by.hour

# Place on grid.
grid.arrange(humidity.weather.scatter, humidity.by.hour, ncol = 2)




###



# ---- windspeed ----------------------------------
# Count by Wind Speed : Scatter Plot
# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# only 30 distinct values, makes this goofy.

mean.windspeed.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$windspeed), FUN = mean)
names(mean.windspeed.count) <- c("windspeed", "meancount")

# Count by Hour (mean) : Bar chart
windspeed.bar <- ggplot(mean.windspeed.count) + geom_bar(aes(x = windspeed, y = meancount), 
                                                           colour = "black", fill = "blue", 
                                                           alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "mean count by windspeed", x = "windspeed", y = "mean count")
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
  labs(x = "Wind Speed (units not provided)", y = "Count", title = "Count by Wind Speed - with mean count trendline") +
  # geom_line(data = mean.windspeed.count, aes(x = windspeed, y = meancount), size = 1, color = "grey28") +
  geom_smooth(data = mean.windspeed.count, aes(x = windspeed, y = meancount))


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
  # theme(legend.position = top) +
  labs(title = "Pro sports events & combined 'sporting_event'")


## Boxplot: sporting_event
sportinghours.any.box <- ggplot(bikeplot.df,
    aes(x = sporting_event, y = count, fill = sporting_event)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  theme(legend.position = "none") +
  labs(title = "sporting_event: all hours")


# Boxplot: sporting_event only during comparable times of day
sportinghours.df <- subset(bikeplot.df, (hour > 17) | (hour > 11 & hour < 16))
sportinhours.comp.box <- ggplot(sportinghours.df, 
       aes(x = sporting_event, y = count, fill = sporting_event)) + 
  geom_boxplot() +
  #theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "sporting_event: noon-15:59 or after 17:59")

  
grid.arrange(sportinghours.any.box, sportinhours.comp.box, ncol = 2)

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
grid.arrange(session_count.histo, session_count.box, session_any.box, ncol=3)

