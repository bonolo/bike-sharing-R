# -- Intro / Header ---------------
# CIS 575 Final Project : Fall 2019
# Kevin F Cullen (solo)

setwd("~/Projects/cis575/bike-sharing")

library(Hmisc)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(pastecs)
library(reshape2)
library(forecast)
library(mgcv)
library(randomForest)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(Metrics)

options(scipen = 100, digits = 6)


# --------- Load CSV file. MySQL export is in one file, with a binary flag ---------------
# 6493 test.csv
# 10886 train.csv
# 17379 total

bikeall.df <- read.csv("csv-inputs/kaggle_data_plus.csv", na.strings = "\\N", header = TRUE)

# Set `peak` binary predictor
bikeall.df$peak <- bikeall.df$hour > 8 & bikeall.df$hour < 21

# derive a column which is true if either house or senate is true.
bikeall.df$congress_both <- ifelse(bikeall.df$house == '1' | bikeall.df$senate == '1', 1, 0)
bikeall.df[,'congress_both'] <- factor(bikeall.df[,'congress_both'])


# Convert a few things to factors
#  (but keep numeric copies of a few first)
bikeall.df[,'house_num']<-as.numeric(bikeall.df[,'house'])
bikeall.df[,'senate_num']<-as.numeric(bikeall.df[,'senate'])
bikeall.df[,'session_any_num']<-as.numeric(bikeall.df[,'session_any'])

bikeall.df[,'dayofweek']<-factor(bikeall.df[,'dayofweek'])
bikeall.df[,'month']<-factor(bikeall.df[,'month'])
bikeall.df[,'is_daylight']<-factor(bikeall.df[,'is_daylight'])
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
# -- Scaling ---------------
# make a copy with columns we want to scale
columns.to.scale <- c("hour", "dayofweek", "month", "season", "weather",
                      "temp", "temp_squared", "atemp", "humidity", "windspeed")
scaled_all.df <- subset(bikeall.df, select = columns.to.scale)

# Convert some of those columns back to numerics
scaled_all.df[,'dayofweek'] <- as.numeric(scaled_all.df[,'dayofweek'])
scaled_all.df[,'month'] <- as.numeric(scaled_all.df[,'month'])
scaled_all.df[,'season'] <- as.numeric(scaled_all.df[,'season'])
scaled_all.df[,'weather'] <- as.numeric(scaled_all.df[,'weather'])

# Scale the numeric variables.
maxs <- apply(scaled_all.df, 2, max)
mins <- apply(scaled_all.df, 2, min)
scaled_all.df <- as.data.frame(scale(scaled_all.df, center = mins, scale = maxs - mins))

# Prepend 'scaled_' to scaled variables
colnames(scaled_all.df) <- paste("scaled", colnames(scaled_all.df), sep = "_")

# ... and recombine with main dataframe
bikeall.df <- data.frame(bikeall.df, scaled_all.df)
rm(scaled_all.df)


# -- Dataframe TEST - submit to Kaggle for scoring
biketest.df <- subset(bikeall.df, train == 0)


# -------------- Data shape & summary ----------------------------
# dim(bikeall.df)
# head(bikeall.df)
# 
# stat.desc(bikeall.df)
# 
# summary(bikeall.df)
# median(bikeall.df$count)
# 
# str(bikeall.df)
# describe(bikeall.df)
# describe(bikeall.df$count)
# 
# describe(as.factor(bikeall.df$temp))
# describe(as.factor(bikeall.df$atemp))
# describe(as.factor(bikeall.df$humidity))
# 
# glimpse(bikeall.df)

