USE bike_sharing;

/* 
 * CREATE tables to hold data arriving via external ETL jobs.
 * 
 * First table holds data from test.csv & train.csv
 *
 */

CREATE TABLE bike_sharing.kaggle_data
(
    training TINYINT
    ,  datetime DATETIME
    , season BIGINT
    , holiday BIGINT
    , workingday BIGINT
    , weather BIGINT
    , temp DOUBLE
    , atemp DOUBLE
    , humidity BIGINT
    , windspeed DOUBLE
    , casual BIGINT
    , registered BIGINT
    , count BIGINT
)
;

select * from bike_sharing.kaggle_data order by datetime asc;
select * from bike_sharing.kaggle_data where workingday not in (1, 0);
SELECT kaggle_data.`datetime`, DAYOFWEEK(kaggle_data.`datetime`) AS dayofweek FROM kaggle_data;
SELECT * FROM kaggle_data WHERE atemp > 40 ORDER BY atemp DESC;

SELECT COUNT(*) FROM kaggle_data WHERE casual IS NULL;
SELECT COUNT(*) FROM kaggle_data WHERE registered IS NULL;
SELECT COUNT(*) FROM kaggle_data WHERE `count` IS NULL;
;
SELECT * FROM kaggle_data WHERE DAYOFMONTH(`datetime`) > 19;
SELECT * FROM kaggle_data WHERE DAYOFMONTH(`datetime`) < 20;
SELECT COUNT(*) FROM kaggle_data WHERE DAYOFMONTH(`datetime`) < 20;


SELECT DISTINCT kaggle_data.temp FROM kaggle_data ORDER BY temp;
SELECT DISTINCT kaggle_data.atemp FROM kaggle_data ORDER BY atemp;
SELECT DISTINCT kaggle_data.humidity FROM kaggle_data ORDER BY humidity;
SELECT DISTINCT kaggle_data.windspeed FROM kaggle_data ORDER BY windspeed;


-- data from house-senate-in-session.csv

CREATE TABLE bike_sharing.house_senate
(
      congress_date DATETIME
    , house INT
    , senate INT
)
;

select * from bike_sharing.house_senate; -- order by datetime asc;

-- data from dc-university-sessions.csv

CREATE TABLE bike_sharing.university_sessions
(
      Date DATETIME
    , cua_session INT
    , au_session INT
    , howard_session INT
    , session_count INT
, session_any INT
)
;

select * from bike_sharing.university_sessions;



-- data from capitals-schedule-2011.csv
CREATE TABLE bike_sharing.capitals_schedule
(
  capitals TINYINT
, caps_date DATETIME
, caps_time TIME
)
;

SELECT * FROM bike_sharing.capitals_schedule;


-- data from nationals-schedule-2011.csv
CREATE TABLE bike_sharing.nationals_schedule
(
      Date INT
    , Nationals_Game INT
    , Nationals_Game_Time TINYTEXT
)
;

select * from bike_sharing.nationals_schedule;

-- data from dc_united-2011-schedule.csv
CREATE TABLE bike_sharing.dc_united_schedule
(
    DC_United_Game_date DATETIME
    , time_ET TIME
)
;
SELECT * from dc_united_schedule;
-- data from washington-wizards-2011-schedule.csv

CREATE TABLE bike_sharing.wizards_schedule
(
    start_ET TIME
    , Date_iso DATETIME
)
;

SELECT * from bike_sharing.wizards_schedule;

/*
 * CREATE table to hold sporting events pulled from other tables,
 * then INSERT INTO... SELECT records from individual sports tables.
 *
 */

CREATE TABLE bike_sharing.sporting_event
(
    `datetime` DATETIME
    # , sporting_event TINYINT
    , capitals TINYINT
    , nationals TINYINT
    , united TINYINT
    , wizards TINYINT
)
;
TRUNCATE TABLE bike_sharing.sporting_event;

SELECT * from bike_sharing.sporting_event ORDER BY datetime;

INSERT INTO sporting_event (`datetime`, capitals)
SELECT CONCAT(LEFT(caps_date, 10), " ", DATE_FORMAT(caps_time, "%H:00:00")) AS `datetime`
, 1 AS capitals
FROM capitals_schedule
ORDER BY caps_date;

INSERT INTO sporting_event (`datetime`, united)
SELECT CONCAT(LEFT(DC_United_Game_date, 10), " ", DATE_FORMAT(time_ET, "%H:00:00")) AS `datetime`
, 1 AS united
FROM dc_united_schedule
ORDER BY DC_United_Game_date;

INSERT INTO sporting_event (`datetime`, wizards)
SELECT CONCAT(LEFT(Date_iso, 10), " ", start_ET) AS `datetime`
    , 1 AS wizards
FROM bike_sharing.wizards_schedule
;

INSERT INTO sporting_event (`datetime`, nationals)
SELECT CONCAT(SUBSTRING(date, 1, 4)
    , "-", SUBSTRING(date, 5, 2)
    , "-", SUBSTRING(date, 7,2)
    , " ", IF(Nationals_Game_Time = 'D', '13:00:00', '19:00:00')) AS `datetime`
    , 1 AS nationals
FROM bike_sharing.nationals_schedule
;


/*******************************
 * We only have the starting hour of each sporting even marked, we need to 
 * INSERT records for the full length of the event, plus pading 
 * of an hour before and after for transit, parking, seating.
 * 
 * Average NBA game length is ~2.25 hours. MLS 110 min. 
 * Hockey 2.5 hours. Baseball: 3 hours.
 * Baseball games are longer, but don't people often leave early?
 *******************************/

-- 1 hour beforehand for transit/parking/seating
INSERT INTO bike_sharing.sporting_event (`datetime`, capitals, nationals, united, wizards)
SELECT DATE_ADD(datetime, INTERVAL -1 HOUR) AS `datetime`
    , capitals, nationals, united, wizards
FROM bike_sharing.sporting_event;
;
-- Add the 2nd hour of the game and 1 hour to get from stadium to home.
INSERT INTO bike_sharing.sporting_event (`datetime`, capitals, nationals, united, wizards)
SELECT DATE_ADD(datetime, INTERVAL 2 HOUR) AS `datetime`
    , capitals, nationals, united, wizards
FROM bike_sharing.sporting_event;
;

SELECT MID(`datetime`, 12, 2) AS start_hour
, COUNT(*) AS frequency
FROM bike_sharing.sporting_event
GROUP BY start_hour
ORDER BY start_hour;

-- Group stuff together, for overlapping events;
    SELECT `datetime`
    , COUNT(capitals) AS capitals
    , COUNT(nationals) AS nationals
    , COUNT(united) AS united
    , COUNT(wizards) AS wizards
    , 1 AS sporting_event
    FROM bike_sharing.sporting_event
    GROUP BY `datetime`
    ;



/****************************
 * Join everything together and export to one flat files with 
 * a binary "training" field.
 */
 -- Get a row of headers so we can put them in CSV export
SELECT 'train', 'datetime', 'hour', 'dayofweek', 'season', 'holiday', 'workingday', 'weather', 'temp', 'atemp'
, 'humidity', 'windspeed', 'casual', 'registered', 'count', 'house', 'senate', 'capitals', 'nationals'
, 'united', 'wizards', 'sporting_event', 'cua_session', 'au_session', 'howard_session', 'session_count', 'session_any'

-- UNION the header row with the data
UNION ALL

SELECT IF(DAYOFMONTH(kaggle_data.`datetime`) < 20, 1, 0) AS train
, kaggle_data.`datetime`
, HOUR(kaggle_data.`datetime`) AS hour
, DAYOFWEEK(kaggle_data.`datetime`) AS dayofweek
, kaggle_data.season
, kaggle_data.holiday
, kaggle_data.workingday
, kaggle_data.weather
, kaggle_data.temp
, kaggle_data.atemp
, kaggle_data.humidity
, kaggle_data.windspeed
, kaggle_data.casual
, kaggle_data.registered
, kaggle_data.`count`
, IFNULL(house_senate.house, 0) AS house
, IFNULL(house_senate.senate, 0) AS senate
, IFNULL(sp_event.capitals, 0) AS capitals
, IFNULL(sp_event.nationals, 0) AS nationals
, IFNULL(sp_event.united, 0) AS united
, IFNULL(sp_event.wizards, 0) AS wizards
, IFNULL(sp_event.sporting_event, 0) AS sporting_event
, IFNULL(university_sessions.cua_session, 0) AS cus_session
, IFNULL(university_sessions.au_session, 0) AS au_session
, IFNULL(university_sessions.howard_session, 0) AS howard_session
, IFNULL(university_sessions.session_count, 0) AS session_count
, IFNULL(university_sessions.session_any, 0) AS session_any

FROM bike_sharing.kaggle_data

LEFT OUTER JOIN bike_sharing.house_senate
    ON LEFT(kaggle_data.`datetime`, 10) = LEFT(house_senate.congress_date, 10)
LEFT OUTER JOIN
(
    SELECT `datetime`
    , COUNT(capitals) AS capitals
    , COUNT(nationals) AS nationals
    , COUNT(united) AS united
    , COUNT(wizards) AS wizards
    , 1 AS sporting_event
    FROM bike_sharing.sporting_event
    GROUP BY `datetime`
) AS sp_event
    ON kaggle_data.`datetime` = sp_event.`datetime`


LEFT OUTER JOIN bike_sharing.university_sessions
    ON LEFT(kaggle_data.`datetime`, 10) = LEFT(university_sessions.`date`, 10)

INTO OUTFILE '/Users/kcullen/Projects/cis575/bike-sharing/csv-inputs/kaggle_data_plus.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
  LINES TERMINATED BY '\n'
;

