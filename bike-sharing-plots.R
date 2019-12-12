# -- Plots ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

# Run `bike-sharing-setup.R` first.
# That's where the library() calls and CSV reads live.
source('bike-sharing-setup.R')

theme_set(theme_light())


# All the vars for plotting. ----------------

# Using train == 1 because we need `count`
bikeplot.df <- subset(bikeall.df, train == 1)

# Shave off the highest outliers in count
bikeplot.df$count_shaved <- bikeplot.df$count
bikeplot.df$count_shaved[bikeplot.df$count_shaved > 
                           quantile(bikeplot.df$count_shaved, c(.90))] <- quantile(bikeplot.df$count_shaved, c(.90))

# Set `peak` binary predictor
bikeplot.df$peak = bikeplot.df$hour > 8 & bikeplot.df$hour < 21


# Declare some variables
boxplot.binary.colors <- c("#E69F00", "#56B4E9")
seasons <- c("Spring", "Summer", "Fall", "Winter")
days.of.week <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
workingday.colors <- c("blue3", "darkorange2") # color per workday/not
holiday.colors <- c("0" = "blue", "1" = "red")
weather.colors <- c("blue", "orange", "red", "black") # color per level of weather
# - weather: (categorical)
# - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
# - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
# - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
# - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog




# Leadoff Graphic: Histogram of count ----------------------
count.histo <- ggplot() + geom_histogram(data = bikeplot.df, aes(x = count), color = "grey33",
                          fill = "olivedrab",alpha = 0.3, binwidth = 25) +
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  expand_limits(x = 1000, y = 1500) +
  labs(x = "count", y = "Frequency", title = "Histogram: count")

peak.histo <- ggplot(data = bikeplot.df[bikeplot.df$peak == TRUE,]) +
  geom_histogram(aes(x = count), color = "grey33",
                 fill = "darkgoldenrod3", alpha = 0.3, binwidth = 25) +
  expand_limits(x = 1000, y = 1500) +
  labs(y = "Frequency", title = "peak hours")

offpeak.histo <- ggplot() + geom_histogram(data = subset(bikeplot.df, hour < 9 | hour > 20), aes(x = count), color = "grey33",
                          fill = "royalblue3",alpha = 0.3, binwidth = 25) +
  expand_limits(x = 1000, y = 1500) +
  labs(y = "Frequency", title = "offpeak hours")
  
grid.arrange(count.histo, peak.histo, offpeak.histo, ncol = 3)
rm(count.histo, peak.histo, offpeak.histo)


# hour & dayofweek ----------------------------------

# Bar chart : count by dayofweek ---------
dayofweek.bar.data <- group_by(bikeplot.df, dayofweek)
dayofweek.bar.data <- summarise(dayofweek.bar.data, mean = mean(count), median = median(count))

dayofweek.bar.data <- melt(dayofweek.bar.data, id.vars = 'dayofweek')
dayofweek.bar <- ggplot() + 
  geom_bar(data = dayofweek.bar.data,
           aes(x = dayofweek, y = value, fill = variable),
           color = "grey33", 
           stat = 'identity', position = 'dodge', alpha = 0.7) + 
  scale_x_discrete(labels = days.of.week) +
  theme(legend.position="top") +
  labs(title = "count by dayofweek", y = "count")
# coord_flip()
dayofweek.bar

rm(dayofweek.bar.data)


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

rm(hour.tile.data)


# JITTERy Scatter Plot : (color: workingday)
hour.scatter2 <- ggplot(bikeplot.df, aes(hour, count, color = workingday)) +
  geom_smooth(se = FALSE) +
  geom_point(aes(x = jitter(hour, 2), y = count),
             pch = 16, alpha = 0.1) + 
  scale_y_sqrt() +
  scale_color_manual(values = workingday.colors) +
  theme(legend.position="top") +
  labs(title = "count by hour | color: workingday")

# Place on grid.
grid.arrange(dayofweek.bar, hour.day.tile, hour.scatter2, ncol = 3) # , widths = c(1, 2, 2))




# dayofweek --------------------------- 
# JITTERy Scatter Plot: count by dayofweek (Holidays in red) 
ggplot(bikeplot.df) + geom_point(aes(x = jitter(as.numeric(dayofweek), 2), y = count, colour = bikeplot.df$holiday), 
                                 pch = 16, alpha = 0.3) +
  scale_color_manual(values = holiday.colors) +
  labs(title = "count by Day of Week (with Holidays in red)", 
       x = "Day of Week (1-7 | Sun - Sat)", 
       y = "count", 
       color = "Holiday?")


# -------  holiday & is_daylight BOX PLOT --------------

holiday_light.df <- bikeplot.df[c("holiday", "is_daylight", "count")]
holiday_light.df <- melt(holiday_light.df, measure.vars = c("holiday", "is_daylight"))

ggplot(holiday_light.df, aes(x = variable, y = count, fill = value)) +
  geom_boxplot() +
  scale_fill_manual(values = boxplot.binary.colors) +
  labs(title = "holiday and is_daylight")

rm(holiday_light.df)


# --------- WEATHER AND SEASON --------- 
## side-by-side boxplots

weather.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = weather, y = count), pch = 20, color = "grey33", fill = "blue", alpha = 0.4) + 
  labs(x = "weather (1-4)", y = "count", title = "count by weather",
       subtitle = "4 = Heavy Rain + Ice Pellets\n + Thunderstorm + Mist, Snow + Fog") +
  theme_light(base_size = 14)

season.box <- ggplot(bikeplot.df) + 
  geom_boxplot(aes(x = season, y = count), pch = 20, color = "grey33", fill = "green", alpha = 0.4) + 
  scale_x_discrete(labels = seasons) +
  labs(y = "count", title = "count by season") +
  theme_light(base_size = 14)

weather.histo <- ggplot(bikeplot.df, (aes(x = weather))) +
  geom_bar(color = "grey33", alpha = 0.5, fill = "blue") +
  labs(title = "Histogram: weather")  +
  theme_light(base_size = 14)

# Place on grid.
grid.arrange(weather.box, weather.histo, season.box, ncol=3)
ggsave("plots/weather-season-boxplot-histo.png",
       arrangeGrob(weather.box, weather.histo, season.box, ncol=3),
       device = "png", units = "in", dpi = 72, scale = 1,
       width = 16, height = 5)




# Temperature (atemp & temp) ----------------------------
# Count rarely low when temp is high 
# - temp: (double) Celsius
# - atemp: (double) "feels like" in Celsius

temp.histo.data <- data.frame(atemp = as.factor(bikeplot.df$atemp), 
                              temp = as.factor(bikeplot.df$temp))
ggplot(data = temp.histo.data, aes(x = temp)) +
  geom_bar(alpha = 0.5, color = "grey33", fill = "red3") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Histogram: temp")


# Scatter Plots: Count by Temperature with mean usage trendline
# atemp
atemp.scatter <- ggplot(bikeplot.df, aes(atemp, count)) +
  geom_point(aes(x = jitter(atemp, 2)), pch = 16, color = "orange", alpha = 0.2) +
  geom_smooth(se = FALSE) +
  geom_line(data = setNames(aggregate(bikeplot.df$count, by = list(bikeplot.df$atemp), FUN = mean), 
                            c("atemp", "meancount")),
            aes(x = atemp, y = meancount)) +
  labs(x = "atemp", y = "count", title = "count by atemp - 'feels like' ºC", 
       subtitle = "mean trendline/loess smoothing")
atemp.scatter



# Peak & Offpeak...
mean.atemp.count <- setNames(aggregate(bikeplot.df$count, 
                                            by = list(bikeplot.df$atemp, bikeplot.df$peak),
                                            FUN = mean),
                                  c("atemp", "peak", "meancount"))
# Peak
atemp.peak.scatter <- ggplot(data = bikeplot.df[bikeplot.df$peak == TRUE,],
                             aes(x = atemp, y = count)) + 
  geom_point(aes(x = jitter(atemp, 2), color = is_daylight), pch = 16, alpha = 0.2) +
  geom_line(data = mean.atemp.count[mean.atemp.count$peak == TRUE,],
            aes(y = meancount), size = 1) +
  geom_smooth(color = "green4",
              method = "gam", formula = y ~ s(x), se = FALSE) +
  scale_color_manual(values = c("navy", "gold3")) +
  theme(legend.position = "top") +
  labs(x = "atemp", y = "count", title = "count vs. atemp @ peak (8 < hour < 21)", 
       subtitle = "method = 'gam', formula = y ~ s(x))")
atemp.peak.scatter

# Offpeak
atemp.offpeak.scatter <- ggplot(data = bikeplot.df[bikeplot.df$peak == FALSE,],
                                aes(x = atemp, y = count)) + 
  geom_point(aes(x = jitter(atemp, 2), color = is_daylight),
             pch = 16, alpha = 0.2) +
  geom_line(data = mean.atemp.count[mean.atemp.count$peak == FALSE,], aes(y = meancount), size = 1) +
  geom_smooth(color = "green4",
              method = "gam", formula = y ~ s(x), se = FALSE) +
  expand_limits(y = 1000) +
  scale_color_manual(values = c("navy", "gold3")) +
  theme(legend.position = "top") +
  labs(x = "atemp", y = "count", title = "count vs. atemp @ offpeak (hour < 9 | hour > 20)", 
       subtitle = "method = 'gam', formula = y ~ s(x))")
atemp.offpeak.scatter

grid.arrange(atemp.peak.scatter, atemp.offpeak.scatter, ncol = 2)

rm(mean.atemp.count)


# temp
temp.scatter <- ggplot(bikeplot.df, aes(temp, count)) +
  geom_point(aes(x = jitter(temp, 2)), pch = 16, color = "salmon", alpha = 0.2) +
  geom_smooth(se = FALSE) +
  geom_line(data = setNames(aggregate(bikeplot.df$count, by = list(bikeplot.df$temp), FUN = mean), 
                            c("temp", "meancount")),
            aes(x = temp, y = meancount)) +
  labs(x = "temp", y = "meanount", title = "count by temp (ºC) - w/ mean trend")
# stat_smooth(color = "red", method = gam, formula = y ~ s(x)) +
# stat_smooth(color = "green", method = lm, formula = y ~ x)
temp.scatter


# mean temp by hour of day: Line Plot
temp.line <- ggplot(
  data = 
    setNames(aggregate(bikeplot.df$temp, by = list(bikeplot.df$hour),
                            FUN = mean), c("hour", "meantemp")),
  aes(x = hour, y = meantemp)) +
  geom_line() + ylim(0, 41) +
  labs(title = "temp varies little through day (~ 5ºC)", y = "mean temp", x = "hour (00-23)")


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
  geom_smooth(data = mean.humidity.count, aes(x = humidity, y = meancount), color = "blue", se = FALSE)
  # stat_smooth(data = bikeplot.df, aes(x = humidity, y = count), color = "red", method = gam, formula = y ~ s(x)) +

humidity.weather.scatter
rm(mean.humidity.count)

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

rm(data.for.plot)



# ---- windspeed ----------------------------------
# Count by Wind Speed : Scatter Plot
# <<<<<<< WIND KILLS DEMAND (Or is it never windy?) <<<<<<<<<<<<<<<<<<<<<<<<
# only 30 distinct values, makes this goofy.

mean.windspeed.count <- aggregate(bikeplot.df$count, by = list(bikeplot.df$windspeed), FUN = mean)
names(mean.windspeed.count) <- c("windspeed", "meancount")

# Count by Hour (mean) : Bar chart
windspeed.bar <- ggplot(mean.windspeed.count) + geom_bar(aes(x = windspeed, y = meancount), 
                                                           colour = "grey33", fill = "blue", 
                                                           alpha = 0.5, stat = "identity") + 
  # scale_y_log10(breaks = c(5, 25, 100, 250, 500, 1000, 1500)) + 
  labs(title = "mean count by windspeed", x = "windspeed", y = "mean count")
windspeed.bar


windspeed.scatter <- ggplot() + 
  geom_point(data = bikeplot.df, 
             aes(x = jitter(bikeplot.df$windspeed, 6), y = count),
             pch = 20,
             # colour = bikeplot.df$weather,
             # colour = bikeplot.df$hour %/% 5 + 1,
             colour = "green4",
             alpha = 0.2) + 
  scale_y_sqrt() +
  # scale_color_manual(values=weather.colors) + 
  labs(x = "Wind Speed (units not provided)", y = "count", title = "count by Wind Speed - with mean count trendline") +
  # geom_line(data = mean.windspeed.count, aes(x = windspeed, y = meancount), size = 1, color = "grey28") +
  geom_smooth(data = mean.windspeed.count, aes(x = windspeed, y = meancount), se = FALSE)

rm(mean.windspeed.count)


# Histogram of windspeed speed
wind.histo <- ggplot(bikeplot.df) + geom_histogram(aes(x = windspeed), colour = "grey33",
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

rm(congress.df, measure.vars, keeps)


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

rm(sports.df, measure.vars, keeps)


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

rm(unis.df, measure.vars, keeps)


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
  scale_fill_manual(values = boxplot.binary.colors) +
  theme(legend.position = "top") +
  labs(title = "session_any is not a proxy for season")

# Use geom_bar instead of geom_histogram to avoid Error: StatBin requires a 
# continuous x variable: the x variable is discrete. Perhaps you want stat="count"?
session_count.histo <- ggplot(data = bikeplot.df, (aes(x = session_count))) +
  geom_bar(fill = "slateblue", color = "grey33", alpha = 0.7) +
  labs(title = "Histogram of session_count")

# Place on grid.
grid.arrange(session_count.histo, session_count.box, session_any.box, ncol=3)

