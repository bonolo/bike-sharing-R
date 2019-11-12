# Project Proposal - CIS 575
## Kevin F. Cullen

## Part 1 A

### The problem/opportunity

I would like to use the Kaggle "Bike Sharing Demand" competition as a basis for my project: <https://www.kaggle.com/c/bike-sharing-demand/overview>.

The data appears in several competitions on Kaggle, but I will be looking at what appears to be the earliest & most detailed version.

Bike sharing companies must balance availability and demand of their vehicles to ensure (a) customers can borrow a bike when they need it and (b) bikes are repaired and in in good order.

I would love to have something better to work on, but my current job (stay-at-home parent) provides more anecdotes than data.

### Business objective(s)

Predict demand for bicycles, using time (date, time, holiday, weekday, season) and weather data. The various competitions do not make true business cases for the prediction based on ROI, etc. However, common sense dictates that sharing companies would want to use this data to keep costs low by having the lowest number of bicycles needed to meet demand. Predictions could also be used to schedule bicycle service and relocation for slower times. (I have read about gig workers whose job is to move ride share bikes and scooters to more central locations.)

### Predictive modeling task(s)

Tasks involved will be:

- Examine the shape of the data through visualizations.
- Cleaning up data and creating some derived variables.
  - e.g. Parse a new "day of week" variable from time stamps
  - look for and account for missing values
  - convert some double-point numbers into integers (rounding) or bins
- Consider whether some continuous weather variables should be converted to variance, etc.
- Build a few models within my limited capabilities. Decision trees would be handy. Maybe we will learn new tools before the assignment is due.

### Potential dataset(s)

I will use the historical dataset provided in the Kaggle competition: <https://www.kaggle.com/c/bike-sharing-demand/data>. The original source is the Capital Bikeshare program in Washington, D.C.

One concern I have about the data is that the training set uses days 1-19 of each month and the validation set uses days 20-end of each month. This troubles me because many people's available funds vary through the month. (I lived in a country where everyone was paid at the end of the month and would generally be broke the last ten days or so.) Availability of funds could easily impact choice of transit mode.


### Approximate number of cases dataset

17,500 cases

### Approximate number of cases you plan to use for i) training and ii) validation

- Test: 6494 cases
- Training: 10,900


### Potential target/response/dependent variable(s)

- casual: rentals by casual users
- registered: rentals by registered users
- **count**: rentals by all users

I will likely focus on the "count" variable. I'm not sure why the type of user would matter, since a bike is a bike.

### Potential predictor/explanatory/independent variables

- datetime: hourly date/time
- season: (categorical) 1 = spring, 2 = summer, 3 = fall, 4 = winter
- workingday: (binary) 1 if neither a holiday or weekend
- weather: (categorical)
  - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
  - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
  - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
  - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
- temp: (double) Celsius
- atemp: (double) "feels like" in Celsius
- humidity (integer) - relative humidity
  - this might be a bit redundant, as weather and atemp could capture this to a degree
- windspeed (double) wind speed, though units are not provided

It would be interesting to add in schedules for major sporting and cultural events, but I don't know if I will have time to gather and format that sort of data.


### Data mining techniques I am considering

I will definitely try decision trees. I will try whatever other tools we learn later in the course. RFM is out of the question, as our cases are based on rental totals by hour, rather than customer cases.

### Data mining software I am considering

My first choice would be R. I may use SAS Enterprise Guide. I will likely use MySQL to view data, clean it up and create derived variables.


## Part 1 B - Sample Data

Found in file: sample-data-kevin-cullen.csv

As mentioned, the competition already has the data divided into training and test data. I created this sample from the training data so you could see the response variables. I just took the header and first 100 records via this command...

$ head -101 train.csv > sample-data-kevin-cullen.csv