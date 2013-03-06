
module Date where

import Native.Date

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec


read : String -> Date
read = Native.Date.read

toTime : Date -> Time
toTime = Native.Date.toTime


year : Date -> Int
year = Native.Date.year

month : Date -> Month
month = Native.Date.month

day : Date -> Int
day = Native.Date.day

dayOfWeek : Date -> Day
dayOfWeek = Native.Date.dayOfWeek

hour : Date -> Int
hour = Native.Date.hour

minute : Date -> Int
minute = Native.Date.minute

second : Date -> Int
second = Native.Date.second
