module Year2018.Day01 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
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
import Util.Util (getFirstRepeating)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = many1' $ fromJust . toBoundedInteger <$> scientific <* (endOfLine <|> endOfInput)

------------ PART A ------------

-- Part A:
-- 445
-- (0.000009s)
partA = sum

------------ PART B ------------

-- Part B:
-- Just 219
-- (0.088431s)
partB :: [Int] -> Maybe Int
partB = getFirstRepeating . scanl (+) 0 . cycle