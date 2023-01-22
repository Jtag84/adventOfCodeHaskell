module Year2018.Day06 (runDay) where

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
import Data.Sequence (mapWithIndex, sort)
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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type MinX = Int 
type MaxX = Int
type MinY = Int
type MaxY = Int

type ClosestPoint = Coordinate
------------ PARSER ------------

inputParser :: Parser [Coordinate]
inputParser = many1' coordinateParser

coordinateParser :: Parser Coordinate
coordinateParser = do
    Just x <- toBoundedInteger <$> scientific
    ", "
    Just y <- toBoundedInteger <$> scientific
    endOfInput <|> endOfLine
    return (XY (x,y))

------------ PART A ------------

getBoundaries :: [Coordinate] -> (MinX, MaxX, MinY, MaxY)
getBoundaries coordinates = do
    let xs = map getX coordinates
    let ys = map getY coordinates
    (minimum xs - 1, maximum xs + 1, minimum ys - 1, maximum ys + 1)

getAllCoordinateWithinBoundaries :: (MinX, MaxX, MinY, MaxY) -> [Coordinate]
getAllCoordinateWithinBoundaries (minX, maxX, minY, maxY) = [XY (x, y) | x <- [minX..maxX], y <- [minY..maxY]]

getCoordinateWithDistances originalCoordinates coordinateWithinBoundary = map (\coordinate -> (coordinate, manhattanDistance coordinateWithinBoundary coordinate)) originalCoordinates

getCoordinateWithShortestManhattanDistance :: [Coordinate] -> Coordinate -> Maybe (Coordinate, ClosestPoint)
getCoordinateWithShortestManhattanDistance originalCoordinates coordinateWithinBoundary = do
    let coordinatesWithMinDistance = head . groupBy (\a b -> snd a == snd b) . sortBy (compare `on` snd) $ getCoordinateWithDistances originalCoordinates coordinateWithinBoundary
    if length coordinatesWithMinDistance == 1
        then
            Just (coordinateWithinBoundary, fst $ head coordinatesWithMinDistance)
        else 
            Nothing

getCoordinatesWithShortestManhattanDistance :: (MinX, MaxX, MinY, MaxY) -> [Coordinate] -> [Maybe (Coordinate, ClosestPoint)]
getCoordinatesWithShortestManhattanDistance boundaries coordinates = do
    let allCoordinatesWithinBoundaries = getAllCoordinateWithinBoundaries boundaries
    map (getCoordinateWithShortestManhattanDistance coordinates) allCoordinatesWithinBoundaries

groupByClosestPointCoordinate :: [Maybe (a, ClosestPoint)] -> [[(a, ClosestPoint)]]
groupByClosestPointCoordinate = L.groupBy ((==) `on` snd) . L.sortBy (compare `on` snd) . catMaybes 

withoutGroupsContainingBoundaryEdges :: (MinX, MaxX, MinY, MaxY) -> [[(Coordinate, ClosestPoint)]] -> [[(Coordinate, ClosestPoint)]]
withoutGroupsContainingBoundaryEdges boundaries = filter (not . any (isOnBoundary boundaries . fst))

isOnBoundary :: (MinX, MaxX, MinY, MaxY) -> Coordinate -> Bool
isOnBoundary (minX, maxX, minY, maxY) coordinate = x == minX || x == maxX || y == minY || y == maxY
    where
        x = getX coordinate
        y = getY coordinate

-- Part A:
-- 3569
-- (0.276059s)
partA :: [Coordinate] -> Int
partA coordinates = do
    let boundaries = getBoundaries coordinates
    let coordinatesWithShortestManhattanDistance = getCoordinatesWithShortestManhattanDistance boundaries coordinates
    maximum . map length . withoutGroupsContainingBoundaryEdges boundaries . groupByClosestPointCoordinate $ coordinatesWithShortestManhattanDistance     

------------ PART B ------------

getSumOfManahattanDistancesFromPoints points = sum . map snd . getCoordinateWithDistances points 

-- Part B:
-- 48978
-- (0.038929s)
partB points = do
    let boundaries = getBoundaries points
    let allCoordinates = getAllCoordinateWithinBoundaries boundaries
    length . filter (< 10000) . map (getSumOfManahattanDistancesFromPoints points) $ allCoordinates
