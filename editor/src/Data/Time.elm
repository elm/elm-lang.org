module Data.Time exposing
  ( withMs, mapMs, mapTime
  , floorMs, floorSecond, floorMinute, floorHour, floorDay, floorMonth, floorYear
  , ceilingMs, ceilingSecond, ceilingMinute, ceilingHour, ceilingDay, ceilingMonth, ceilingYear
  )

import Time
import Time.Extra as T


withMs : (Int -> a) -> Time.Posix -> a
withMs func time =
  func (Time.posixToMillis time)


mapMs : (Int -> Int) -> Time.Posix -> Time.Posix
mapMs func time =
  Time.millisToPosix (withMs func time)


mapTime : (Time.Posix -> Time.Posix) -> Int -> Int
mapTime func time =
  Time.posixToMillis (func (Time.millisToPosix time))



-- FLOOR


floorMs : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorMs zone mult stamp =
  let parts = T.posixToParts zone stamp
      rem = remainderBy mult parts.millisecond
  in
  if rem == 0
  then T.partsToPosix zone parts
  else T.add T.Millisecond -rem zone stamp


floorSecond : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorSecond zone mult stamp =
  let parts = T.posixToParts zone (T.floor T.Second zone stamp)
      rem = remainderBy mult parts.second
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Second -rem zone stamp


floorMinute : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorMinute zone mult stamp =
  let parts = T.posixToParts zone (T.floor T.Minute zone stamp)
      rem = remainderBy mult parts.minute
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Minute -rem zone new


floorHour : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorHour zone mult stamp =
  let parts = T.posixToParts zone (T.floor T.Hour zone stamp)
      rem = remainderBy mult parts.hour
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Hour -rem zone new


floorDay : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorDay zone mult stamp =
  if mult == 7 then
    T.floor T.Week zone stamp
  else
    T.floor T.Day zone stamp


floorMonth : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorMonth zone mult stamp =
  let parts = T.posixToParts zone (T.floor T.Month zone stamp)
      monthInt = monthAsInt parts.month
      rem = remainderBy mult (monthInt - 1)
      newMonth = if rem == 0 then monthInt else monthInt - rem
  in
  T.partsToPosix zone { parts | month = intAsMonth newMonth }


floorYear : Time.Zone -> Int -> Time.Posix -> Time.Posix
floorYear zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Year zone stamp)
      rem = remainderBy mult parts.year
      newYear = if rem == 0 then parts.year else parts.year - rem
  in
  T.partsToPosix zone { parts | year = newYear }



-- CEILING


ceilingMs : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingMs zone mult stamp =
  let parts = T.posixToParts zone stamp
      rem = remainderBy mult parts.millisecond
  in
  if rem == 0
  then T.partsToPosix zone parts
  else T.add T.Millisecond (mult - rem) zone stamp


ceilingSecond : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingSecond zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Second zone stamp)
      rem = remainderBy mult parts.second
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Second (mult - rem) zone new


ceilingMinute : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingMinute zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Minute zone stamp)
      rem = remainderBy mult parts.minute
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Minute (mult - rem) zone new


ceilingHour : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingHour zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Hour zone stamp)
      rem = remainderBy mult parts.hour
      new = T.partsToPosix zone parts
  in
  if rem == 0
  then new
  else T.add T.Hour (mult - rem) zone new


ceilingDay : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingDay zone mult stamp =
  if mult == 7 then
    T.ceiling T.Week zone stamp
  else
    T.ceiling T.Day zone stamp


ceilingMonth : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingMonth zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Month zone stamp)
      monthInt = monthAsInt parts.month -- 12
      rem = remainderBy mult (monthInt - 1) -- 11 % 3 = 2
      newMonth = if rem == 0 then monthInt else monthInt - rem + mult -- 12 - 2 + 3 = 13
  in
  T.partsToPosix zone <|
    if newMonth > 12
    then { parts | year = parts.year + 1, month = intAsMonth (newMonth - 12) }
    else { parts | month = intAsMonth newMonth }


ceilingYear : Time.Zone -> Int -> Time.Posix -> Time.Posix
ceilingYear zone mult stamp =
  let parts = T.posixToParts zone (T.ceiling T.Year zone stamp)
      rem = remainderBy mult parts.year
      newYear = if rem == 0 then parts.year else parts.year - rem + mult
  in
  T.partsToPosix zone { parts | year = newYear }



-- HELP


monthAsInt : Time.Month -> Int
monthAsInt month =
  case month of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12


intAsMonth : Int -> Time.Month
intAsMonth int =
  case int of
    1 -> Time.Jan
    2 -> Time.Feb
    3 -> Time.Mar
    4 -> Time.Apr
    5 -> Time.May
    6 -> Time.Jun
    7 -> Time.Jul
    8 -> Time.Aug
    9 -> Time.Sep
    10 -> Time.Oct
    11 -> Time.Nov
    12 -> Time.Dec
    _ -> Time.Dec

