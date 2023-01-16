module Year2019.Day01 (runDay) where

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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = many1' $ fromJust . toBoundedInteger <$> scientific <* (endOfLine <|> endOfInput)

------------ PART A ------------

-- Part A:
-- 3452245
-- (0.000005s)
partA = sum . map (\mass -> mass `div` 3 - 2)

------------ PART B ------------

calculateFuelForMass mass = do
        let fuel = mass `div` 3 - 2
        if fuel <= 0
            then
                0
            else
                fuel + calculateFuelForMass fuel

-- Part B:
-- 5175499
-- (0.000456s)
partB = sum . map calculateFuelForMass
