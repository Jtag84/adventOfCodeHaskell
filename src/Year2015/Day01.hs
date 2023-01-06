module Year2015.Day01 (runDay) where

import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText)
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

------------ PARSER ------------

inputParser :: Parser [Int]
inputParser =
  many1'
    ( choice
        [ char '(' $> 1,
          char ')' $> (-1)
        ]
    )
    <* (endOfLine <|> endOfInput)

------------ PART A ------------
-- Part A:
-- 280
-- (0.000036s)
partA :: [Int] -> Int
partA = sum

------------ PART B ------------
-- Part B:
-- 1797
-- (0.000085s)
partB :: [Int] -> Int
partB = length . takeUntilSum (-1) 0

takeUntilSum targetSum currentSum [] = []
takeUntilSum targetSum currentSum (x : xs)
  | targetSum == currentSum = []
  | otherwise = x : takeUntilSum targetSum (currentSum + x) xs
