module Years (yearDaysMap) where

import Data.Map (Map, fromList)
import Program.RunDay (Day)
import Year2015.Day01 qualified (runDay)
import Year2015.Day02 qualified (runDay)
import Year2015.Day03 qualified (runDay)
import Year2015.Day04 qualified (runDay)
import Year2015.Day05 qualified (runDay)
import Year2015.Day06 qualified (runDay)
import Year2022.Day01 qualified (runDay)
import Year2022.Day13 qualified (runDay)
import Year2022.Day14 qualified (runDay)
import Year2022.Day15 qualified (runDay)
import Year2022.Day16 qualified (runDay)
import Year2022.Day17 qualified (runDay)
import Year2022.Day18 qualified (runDay)
import Year2022.Day19 qualified (runDay)
import Year2022.Day20 qualified (runDay)
import Year2022.Day21 qualified (runDay)
import Year2022.Day22 qualified (runDay)
import Year2022.Day23 qualified (runDay)
import Year2022.Day24 qualified (runDay)
import Year2022.Day25 qualified (runDay)

type Year = Int

type DayNumber = Int

yearDaysMap :: Map Year (Map DayNumber Day)
yearDaysMap =
  fromList
    [ (2015, year2015),
      (2022, year2022)
    ]

year2015 :: Map DayNumber Day
year2015 =
  fromList
    [ (1, Year2015.Day01.runDay),
      (2, Year2015.Day02.runDay),
      (3, Year2015.Day03.runDay),
      (4, Year2015.Day04.runDay),
      (5, Year2015.Day05.runDay),
      (6, Year2015.Day06.runDay)
    ]

year2022 :: Map DayNumber Day
year2022 =
  fromList
    [ (1, Year2022.Day01.runDay),
      (13, Year2022.Day13.runDay),
      (14, Year2022.Day14.runDay),
      (15, Year2022.Day15.runDay),
      (16, Year2022.Day16.runDay),
      (17, Year2022.Day17.runDay),
      (18, Year2022.Day18.runDay),
      (19, Year2022.Day19.runDay),
      (20, Year2022.Day20.runDay),
      (21, Year2022.Day21.runDay),
      (22, Year2022.Day22.runDay),
      (23, Year2022.Day23.runDay),
      (24, Year2022.Day24.runDay),
      (25, Year2022.Day25.runDay)
    ]
