module Year2015.Day08 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, anyChar, letter, inClass, satisfy)
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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [String]
inputParser = many1' wordParser

wordParser :: Parser String
wordParser = many1' (satisfy (inClass "a-zAZ\\\"0-9")) <* (endOfInput <|> endOfLine)

------------ PART A ------------
countCodeRepresentationChar :: Int -> String -> Int
countCodeRepresentationChar currentCount [] = currentCount
countCodeRepresentationChar currentCount ('\\':'x':c:d:xs) = countCodeRepresentationChar (currentCount + 3) xs
countCodeRepresentationChar currentCount ('\\':b:xs) = countCodeRepresentationChar (currentCount + 1) xs
countCodeRepresentationChar currentCount (x:xs) = countCodeRepresentationChar currentCount xs

-- Part A:
-- 1371
-- (0.000072s)
partA = sum . map (countCodeRepresentationChar 2)

------------ PART B ------------

-- Part B:
-- 2117
-- (0.000158s)
partB = sum . map (\s -> (length . show) s - length s)