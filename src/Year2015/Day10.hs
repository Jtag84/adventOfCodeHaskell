module Year2015.Day10 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, digit)
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

inputParser :: Parser String
inputParser = many1' digit <* (endOfInput <|> endOfLine)

------------ PART A ------------

-- Part A:
-- 252594
-- (0.059498s)
partA = length . applyLookAndSay 40

-- >>> applyLookAndSay 1 "111221"
-- "312211"
-- >>> applyLookAndSay 2 "111221"
-- "13112221"
applyLookAndSay :: Int -> String -> String
applyLookAndSay n s = last . take (n + 1) $ iterate lookAndSay s
    where
        lookAndSay = concatMap (\groupedDigits -> show (length groupedDigits) <> [head groupedDigits]) . group

------------ PART B ------------

-- Part B:
-- 3579328
-- (0.649520s)
partB = length . applyLookAndSay 50