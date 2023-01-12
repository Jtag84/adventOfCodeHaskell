module Year2017.Day01 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, digit)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Sequence (mapWithIndex)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import Data.Char (digitToInt)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = many1' digitParser <* (endOfInput <|> endOfLine)

digitParser :: Parser Int
digitParser = digitToInt <$> digit

------------ PART A ------------

-- Part A:
-- 1102
-- (0.000111s)
partA (x:xs) = sum . map (\digits -> head digits * (length digits - 1)) . group $ (x:xs <> [x])

------------ PART B ------------
-- Part B:
-- 1076
-- (0.010244s)
partB :: [Int] -> Int
partB listOfDigits = do
    let listOfDigitsLength = length listOfDigits
    let stepForwards = listOfDigitsLength `div` 2
    let circularList = cycle listOfDigits
    sum $ zipWith (\index digit -> if digit == (circularList !! (stepForwards+index)) then digit else 0) [0 ..] listOfDigits