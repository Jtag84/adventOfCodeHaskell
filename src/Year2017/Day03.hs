module Year2017.Day03 (runDay) where

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
import Data.Tuple.All (Sel2(sel2), Sel3 (sel3), Sel1 (sel1))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type UpDownIncrement = Int
type LeftRightIncrement = Int
type IncrementRemaining = Int

type SpiralCoordinateState = (UpDownIncrement,LeftRightIncrement, Coordinate, Direction, IncrementRemaining)

------------ PARSER ------------

inputParser :: Parser Int
inputParser = fromJust . toBoundedInteger <$> scientific

------------ PART A ------------

start = XY (0,0)

-- >>> getNextCoordinate (1,1, XY (0,0), RightD, 1)
-- (1,1,XY (1,0),RightD,0)
-- >>> take 5 $ iterate getNextCoordinate (1,1, XY (0,0), RightD, 1)
-- [(1,1,XY (0,0),RightD,1),(1,1,XY (1,0),RightD,0),(1,2,XY (1,-1),UpD,0),(2,2,XY (0,-1),LeftD,1),(2,2,XY (-1,-1),LeftD,0)]
getNextCoordinate :: SpiralCoordinateState -> SpiralCoordinateState
getNextCoordinate (upDownIncrement, leftRightIncrement, coordinate, direction, 0)
    | direction == UpD || direction == DownD = (upDownIncrement + 1, leftRightIncrement, moveInDirection coordinate rotatedDirection 1, rotatedDirection, leftRightIncrement - 1)
    | direction == LeftD || direction == RightD = (upDownIncrement, leftRightIncrement + 1, moveInDirection coordinate rotatedDirection 1, rotatedDirection, upDownIncrement - 1)
    where
        rotatedDirection = rotate CounterClockwise direction
getNextCoordinate (upDownIncrement, leftRightIncrement, coordinate, direction, incrementRemaining) = (upDownIncrement, leftRightIncrement, moveInDirection coordinate direction 1, direction, incrementRemaining - 1)

-- >>> getNthCoordinate 10
-- XY (2,1)
getNthCoordinate :: Int -> Coordinate
getNthCoordinate n = sel3 . last . take n $ iterate getNextCoordinate (1,1, start, RightD, 1)

-- Part A:
-- 552
-- (0.077902s)
partA :: Int -> Int
partA = manhattanDistance start . getNthCoordinate

------------ PART B ------------

getNextCoordinateWithSum :: (Int, Map.Map Coordinate Int, SpiralCoordinateState) -> (Int, Map.Map Coordinate Int, SpiralCoordinateState)
getNextCoordinateWithSum (_, coordinateValueMap, spiralCoordinateState) = do
    let nextSpiraCoordinateState@(_,_,newCoordinate,_,_) = getNextCoordinate spiralCoordinateState
    let aroundCoordinates = getAroundCoordinatesIncludingDiagonals newCoordinate
    let allCurrentValues = map (\coordinate -> Map.findWithDefault 0 coordinate coordinateValueMap) aroundCoordinates
    let sumOfAroundValues = sum allCurrentValues
    let newCoordinateValueMap = Map.insert newCoordinate sumOfAroundValues coordinateValueMap
    (sumOfAroundValues, newCoordinateValueMap, nextSpiraCoordinateState)

-- Part B:
-- 330785
-- (0.000150s)
partB n = sel1 $ until ((>n) . sel1) getNextCoordinateWithSum (1, Map.singleton start 1, (1,1, start, RightD, 1))