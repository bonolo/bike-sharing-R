
# Project - CIS 575
# Part 3 - Final Report - Kevin F. Cullen

(8 to 20 pages excluding appendices)

Relevant output from your analyses should be included in the Appendix and referenced in the body of your report. The due date for the final report is Sunday, December 8.

## Executive summary

insert  
  
content  
  
here  

## Business problem/opportunity (from proposal)

I used the Kaggle "Bike Sharing Demand" competition as a basis for my project: <https://www.kaggle.com/c/bike-sharing-demand/overview>. Several versions of the data are on Kaggle. I chose what seemed the earliest & most detailed.

Bike sharing companies must balance availability and demand of their vehicles to ensure (a) customers can borrow a bike when they need it and (b) bikes are repaired and in in good order.

The Kaggle challenge requests the following, where "count" is the number of bicycles rented in a given hour...

**Evaluation statistic:** Root Mean Squared Logarithmic Error (RMSLE)

**Submission format**

    datetime,count
    2011-01-20 00:00:00,0
    2011-01-20 01:00:00,0
    2011-01-20 02:00:00,0

## Specific business objective(s) (from proposal)

Predict demand for bicycles, using time (date, time, holiday, weekday, season) and weather data. The various competitions do not make true business cases for the prediction based on ROI, etc. However, common sense dictates that sharing companies would want to use this data to keep costs low by having the lowest number of bicycles needed to meet demand. Predictions could also be used to schedule bicycle service and relocation for slower times. (I have read about gig workers whose job is to move ride share bikes and scooters to more central locations.)


## Process followed for selecting and gathering data

### Kaggle Data

Downloaded CSV files for training and test data from <https://www.kaggle.com/c/bike-sharing-demand/overview>.

### Additional Data Series I Found and Transformed

Some of this was ugly cut & paste from PDFs or HTML into Excel. I used Pentaho/Vandara PDI to load CSVs into a MySQL database for transformations and joins via SQL.

I pulled the following subsets together via SQL SELECT with several OUTER JOINs and added derived fields like "hour (of day)", "dayofweek (1-7)". I exported train and test CSV files with MySQL's SELECT... INTO OUTFILE syntax.

Related files...

    create+select.sql       transformations and joins
    bike_sharing.kjb        (Pentaho PDI ETL Job)
    kaggle_data.ktr         (Pentaho PDI Transformation file)
    kaggle_plus_test.csv    unified test data file (kaggle + my additional data)
    kaggle_plus_train.csv   unified test data file (kaggle + my additional data)


#### 1. House and Senate "in session" variables [house-senate-in-session.csv]

Perhaps DC is busier when the legislators are around. I created CSV files from the official calendars of the 112th Congress. <https://www.congress.gov/past-days-in-session> It took 10 minutes and seemed faster than screen-scraping.

Variables

    Date        iso date
    House       binary
    Senate      binary

#### 2. University calendars [dc-university-sessions.csv]

University students are a prime demographic for bike sharing. I wanted to capture days when students were likely in town, but not burdened by exams, etc. I found 2011 calendars for the 3rd, 4th, and 5th-largest universities in the DC area (but not 1st or 2nd). I made a CSV to demark days I considered class to be in session. I included weekends but excluded: Thanksgiving breaks, spring breaks, exam weeks, and summer sessions.

Variables...

    cua_session        binary    Catholic University of America
    au_session         binary    American University
    howard_session     binary    Howard University
    session_count      integer   # of universities in session
    session_any        binary    any universities in session?

#### 3. Pro sports schedules (DC-area home games)

Who drives to a pro sporting event? So, I captured (in CSV files) time windows for professional sporting events held within the geographic footprint of the bike share service. I excluded the Washington Redskins because the play in Landover, Maryland... outside the bike sharing service area.

These were in all sorts of formats. Once the CSVs were in MySQL, I massaged and joined them into a common table.

    +-----------+----------+
    | Field     | Type     |
    +-----------+----------+
    | datetime  | datetime | To join with the Kaggle hourly time data.
    | nationals | boolean  | Is there a game at this time?
    | united    | boolean  | Game?
    | wizards   | boolean  | Game?
    +-----------+----------+

Using start times from the CSV files and typical game lengths, I used SQL to set the binary flag to TRUE for hours during which games were being held, along with a bit of buffer on either side for travel to and from the games. I excluded away games for all.

##### Washington Capitals (NHL) - Never found an easy source

##### Washington Nationals (MLB) [nationals-schedule-2011.csv]

Gathered from: <https://www.retrosheet.org/schedule/>

Variables

    Date                    string (yyyymmdd)
    Nationals_Game          binary
    Nationals_Game_Time     string

I replaced Nationals_Game_Time strings ("D" or "N") with typical game times.
- D (Day) ~13:05
- N (Night) ~19:05


##### Washington Wizards (NBA) [washington-wizards-2011-schedule.csv]

Gathered from: <https://www.basketball-reference.com/teams/WAS/2012_games.html>.

Variables

    start_ET      time (HH:mm:ss)
    Date_iso      iso date

##### DC United (MLS) [dc_united-2011-schedule.csv]

Gathered from: <https://en.wikipedia.org/wiki/2011_D.C._United_season>

Variables

    DC_United_Game_date      iso date
    time_ET                  time (HH:mm:ss)



## Discussion of preliminary data exploration and findings




## Description of data preparation - repairs, replacements, reductions, partitions, derivations, transformations and variable clustering




## Description of data modeling/analyses and assessments



## Explanation of model comparisons and model selection



## Conclusions and recommendations (i.e., what did you learn from the analysis; did you meet your stated business objective(s); how can the results of your analysis address the business problem/opportunity; what further analyses, that builds on your work, can be in done in the future)









