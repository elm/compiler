
module Date where

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

data Month = Jan | Feb | Mar | Apr
           | May | Jun | Jul | Aug
           | Sep | Oct | Nov | Dec

read : String -> Date
toTime : Date -> Time

year  : Date -> Int
month : Date -> Month
day   : Date -> Int
dayOfWeek : Date -> Day
hour   : Date -> Int
minute : Date -> Int
second : Date -> Int
