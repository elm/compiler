
-- Library for working with dates. It is still a work in progress, so email
-- the mailing list if you are having issues with internationalization or
-- locale formatting or something.
module Date where

-- Represents the days of the week.
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

-- Represents the month of the year.
data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec

-- Attempt to read a date from a string.
read : String -> Maybe Date

-- Convert a date into a time since midnight (UTC) of 1 January 1990 (i.e.
-- [UNIX time](http://en.wikipedia.org/wiki/Unix_time)). Given the date 23 June
-- 1990 at 11:45AM this returns the corresponding time.
toTime : Date -> Time

-- Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the integer `1990`.
year  : Date -> Int

-- Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the Month `Jun` as defined below.
month : Date -> Month


-- Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the integer `23`.
day   : Date -> Int

-- Extract the day of the week for a given date. Given the date 23 June
-- 1990 at 11:45AM this returns the Day `Thu` as defined below.
dayOfWeek : Date -> Day

-- Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the integer `11`.
hour   : Date -> Int

-- Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the integer `45`.
minute : Date -> Int

-- Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
-- this returns the integer `0`.
second : Date -> Int
