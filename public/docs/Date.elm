
import Website.Docs (createDocs2)

conversions =
  [ ("read"     , "String -> Maybe Date", [markdown|
Attempt to read a date from a string.|])
  , ("toTime"   , "Date -> Time", [markdown|
Convert a date into a time since midnight (UTC) of 1 January 1990 (i.e.
[UNIX time](http://en.wikipedia.org/wiki/Unix_time)). Given the date 23 June
1990 at 11:45AM this returns the corresponding time.|])
  , ("year"     , "Date -> Int", [markdown|
Extract the year of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `1990`.|])
  , ("month"    , "Date -> Month", [markdown|
Extract the month of a given date. Given the date 23 June 1990 at 11:45AM
this returns the Month `Jun` as defined below.|])
  , ("day"      , "Date -> Int", [markdown|
Extract the day of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `23`.|])
  , ("dayOfWeek", "Date -> Day", [markdown|
Extract the day of the week for a given date. Given the date 23 June
1990 at 11:45AM this returns the Day `Thu` as defined below.|])
  , ("hour"     , "Date -> Int", [markdown|
Extract the hour of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `11`.|])
  , ("minute"   , "Date -> Int", [markdown|
Extract the minute of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `45`.|])
  , ("second"   , "Date -> Int", [markdown|
Extract the second of a given date. Given the date 23 June 1990 at 11:45AM
this returns the integer `0`.|])
  ]

datas =
  [ ("data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat", "", [markdown|
Represents the days of the week.|])
  , ("data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec", "", [markdown|
Represents the month of the year.|])
  ]


categories = [ ("Days and Months", datas)
             , ("Conversions", conversions)
             ]

intro = [markdown|
Library for working with dates. It is still a work in progress, so email
the mailing list if you are having issues with internationalization or
locale formatting or something.
|]

main = createDocs2 "Date" intro categories