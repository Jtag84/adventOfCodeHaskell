module Year2019.Day03 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState, MonadState (get))
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, sepBy')
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
import qualified Data.List as L
import Util.Util (takeUntil)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Line = (Direction, Int)
type Path = [Line]

------------ PARSER ------------

inputParser :: Parser (Path, Path)
inputParser = do
    line1 <- pathParser 
    line2 <- pathParser 
    return (line1, line2)

pathParser :: Parser Path
pathParser = lineParser `sepBy'` "," <* (endOfLine <|> endOfInput)

lineParser :: Parser Line
lineParser = do
    direction <- P.choice [
            char 'U' $> UpD,
            char 'D' $> DownD,
            char 'L' $> LeftD,
            char 'R' $> RightD
        ]
    Just numberForward <- toBoundedInteger <$> P.scientific
    return (direction, numberForward)

------------ PART A ------------
start = XY (0,0)

getCoordinatesFromLine :: Coordinate -> Line -> [Coordinate]
getCoordinatesFromLine coordinate (RightD, numberForward) = [ XY (x, getY coordinate) | x <- [getX coordinate .. getX coordinate + numberForward]]
getCoordinatesFromLine coordinate (LeftD, numberForward)  = [ XY (x, getY coordinate) | x <- reverse [getX coordinate - numberForward .. getX coordinate]]
getCoordinatesFromLine coordinate (UpD, numberForward)    = [ XY (getX coordinate, y) | y <- reverse [getY coordinate - numberForward .. getY coordinate]]
getCoordinatesFromLine coordinate (DownD, numberForward)  = [ XY (getX coordinate, y) | y <- [getY coordinate  .. getY coordinate + numberForward]]

getAllCoordinate :: Coordinate -> Path -> [Coordinate]
getAllCoordinate _ [] = []
getAllCoordinate from (line:lines) = do
    let coordinates = getCoordinatesFromLine from line
    init coordinates <> getAllCoordinate (last coordinates) lines

-- Part A:
-- 8015
-- (0.168006s)
partA (wire1, wire2) = do
    let coordinates1 = Set.fromList $ tail $ getAllCoordinate start wire1
    let coordinates2 = Set.fromList $ tail $ getAllCoordinate start wire2
    minimum . Set.map (manhattanDistance start) $ coordinates1 `Set.intersection` coordinates2

------------ PART B ------------

calculateNumberOfStepsToIntersection :: [Coordinate] -> [Coordinate] -> Coordinate -> Int
calculateNumberOfStepsToIntersection path1 path2 intersectionCoordinate = do
    let stepsInPath1 = length . takeWhile (/= intersectionCoordinate) $ path1
    let stepsInPath2 = length . takeWhile (/= intersectionCoordinate) $ path2
    stepsInPath1 + stepsInPath2

-- Part B:
-- 163676
-- (0.166640s)
partB (wire1, wire2) = do
    let coordinates1 = getAllCoordinate start wire1
    let coordinates2 = getAllCoordinate start wire2
    let intersections = Set.fromList (tail coordinates1) `Set.intersection` Set.fromList (tail coordinates2)
    minimum $ Set.map (calculateNumberOfStepsToIntersection coordinates1 coordinates2) intersections