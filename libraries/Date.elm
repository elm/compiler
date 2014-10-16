
module Date where

{-| Library for working with dates. Email the mailing list if you encounter
issues with internationalization or locale formatting.

# Conversions
@docs read, toTime, fromTime

# Extractions
@docs year, month, Month, day, dayOfWeek, Day, hour, minute, second

-}

import Native.Date
import Time (Time)
import Maybe (Maybe)

type Date = Date

{-| Represents the days of the week. -}
type Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

{-| Represents the month of the year. -}
type Month
    = Jan | Feb | Mar | Apr
    | May | Jun | Jul | Aug
    | Sep | Oct | Nov | Dec

{-| Attempt to read a date from a string. -}
read : String -> Maybe Date
read = Native.Date.read

{-| Convert a date into a time since midnight (UTC) of 1 January 1990 (i.e.
[UNIX time](http://en.wikipedia.org/wiki/Unix_time)). Given the date 23 June
1990 at 11:45AM this returns the corresponding time. -}
toTime : Date -> Time
toTime = Native.Date.toTime

{-| Take a UNIX time and convert it to a `Date`. -}
fromTime : Time -> Date
fromTime = Native.Date.fromTime

{-| Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `1990`. -}
year : Date -> Int
year = Native.Date.year

{-| Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
this returns the Month `Jun` as defined below. -}
month : Date -> Month
month = Native.Date.month

{-| Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `23`. -}
day : Date -> Int
day = Native.Date.day

{-| Extract the day of the week for a given date. Given the date 23 June
1990 at 11:45AM this returns the Day `Thu` as defined below. -}
dayOfWeek : Date -> Day
dayOfWeek = Native.Date.dayOfWeek

{-| Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `11`. -}
hour : Date -> Int
hour = Native.Date.hour

{-| Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `45`. -}
minute : Date -> Int
minute = Native.Date.minute

{-| Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `0`. -}
second : Date -> Int
second = Native.Date.second
