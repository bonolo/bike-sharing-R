

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