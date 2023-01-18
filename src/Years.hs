module Years (yearDaysMap) where

import Data.Map (Map, fromList)
import Program.RunDay (Day)
import Year2015.Day01 qualified (runDay)
import Year2015.Day02 qualified (runDay)
import Year2015.Day03 qualified (runDay)
import Year2015.Day04 qualified (runDay)
import Year2015.Day05 qualified (runDay)
import Year2015.Day06 qualified (runDay)
import Year2015.Day07 qualified (runDay)
import Year2015.Day08 qualified (runDay)
import Year2015.Day09 qualified (runDay)
import Year2015.Day10 qualified (runDay)
import Year2015.Day11 qualified (runDay)
import Year2015.Day12 qualified (runDay)
import Year2015.Day13 qualified (runDay)
import Year2015.Day14 qualified (runDay)
import Year2015.Day15 qualified (runDay)
import Year2016.Day01 qualified (runDay)
import Year2016.Day02 qualified (runDay)
import Year2016.Day03 qualified (runDay)
import Year2016.Day04 qualified (runDay)
import Year2016.Day05 qualified (runDay)
import Year2017.Day01 qualified (runDay)
import Year2017.Day02 qualified (runDay)
import Year2017.Day03 qualified (runDay)
import Year2017.Day04 qualified (runDay)
import Year2017.Day05 qualified (runDay)
import Year2018.Day01 qualified (runDay)
import Year2018.Day02 qualified (runDay)
import Year2018.Day03 qualified (runDay)
import Year2018.Day04 qualified (runDay)
import Year2018.Day05 qualified (runDay)
import Year2019.Day01 qualified (runDay)
import Year2019.Day02 qualified (runDay)
import Year2019.Day03 qualified (runDay)
import Year2019.Day04 qualified (runDay)
import Year2019.Day05 qualified (runDay)
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
      (2022, year2022),
      (2016, year2016),
      (2017, year2017),
      (2018, year2018),
      (2019, year2019)
    ]

year2015 :: Map DayNumber Day
year2015 =
  fromList
    [ (1, Year2015.Day01.runDay),
      (2, Year2015.Day02.runDay),
      (3, Year2015.Day03.runDay),
      (4, Year2015.Day04.runDay),
      (5, Year2015.Day05.runDay),
      (6, Year2015.Day06.runDay),
      (7, Year2015.Day07.runDay),
      (8, Year2015.Day08.runDay),
      (9, Year2015.Day09.runDay),
      (10, Year2015.Day10.runDay),
      (11, Year2015.Day11.runDay),
      (12, Year2015.Day12.runDay),
      (13, Year2015.Day13.runDay),
      (14, Year2015.Day14.runDay),
      (15, Year2015.Day15.runDay)
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

year2016 :: Map DayNumber Day
year2016 =
  fromList
    [ (1, Year2016.Day01.runDay),
      (2, Year2016.Day02.runDay),
      (3, Year2016.Day03.runDay),
      (4, Year2016.Day04.runDay),
      (5, Year2016.Day05.runDay)
    ]

year2017 :: Map DayNumber Day
year2017 =
  fromList
    [ (1, Year2017.Day01.runDay),
      (2, Year2017.Day02.runDay),
      (3, Year2017.Day03.runDay),
      (4, Year2017.Day04.runDay),
      (5, Year2017.Day05.runDay)
    ]

year2018 :: Map DayNumber Day
year2018 =
  fromList
    [ (1, Year2018.Day01.runDay),
      (2, Year2018.Day02.runDay),
      (3, Year2018.Day03.runDay),
      (4, Year2018.Day04.runDay),
      (5, Year2018.Day05.runDay)
    ]

year2019 :: Map DayNumber Day
year2019 =
  fromList
    [ (1, Year2019.Day01.runDay),
      (2, Year2019.Day02.runDay),
      (3, Year2019.Day03.runDay),
      (4, Year2019.Day04.runDay),
      (5, Year2019.Day05.runDay)
    ]
