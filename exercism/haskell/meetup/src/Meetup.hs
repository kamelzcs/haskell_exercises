module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate
import Control.Monad

-- The task is to create the data types `Weekday` and
-- `Schedule`, and implement the function `meetupDay`.

data Schedule = Teenth | First | Second | Third | Fourth | Last
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving(Enum)



meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = head $
        do day <- ranges
           let dayTime = fromGregorian year month day
               weekDay = fst3 (toWeekDate dayTime)
           guard (weekDay == (fromEnum weekday) + 1)
           return dayTime
           where ranges = case schedule of Teenth -> [13..19]
                                           First -> [1..7]
                                           Second -> [8..14]
                                           Third -> [15..21]
                                           Fourth -> [22..28]
                                           Last -> [totalDays - 6 .. totalDays]
                 fst3 (_, _, s) = s
                 totalDays = gregorianMonthLength year month
