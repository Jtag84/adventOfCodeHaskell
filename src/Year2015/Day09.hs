module Year2015.Day09 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, space, takeText, letter, takeTill)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List as L hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger, scientific)
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
import Data.Char (isLetter)

runDay :: R.Day
runDay = R.runDay distancesParser partA partB

------------ TYPES ------------
type City = Text
type Distance = Int

type Distances = Map.Map (City, City) Distance
type NodeState = (City, Set.Set City)
------------ PARSER ------------

cityParser :: Parser City
cityParser = takeTill (not . isLetter)

distanceParser :: Parser ((City, City), Distance)
distanceParser = do
    city1 <- cityParser
    " to "
    city2 <- cityParser
    " = "
    Just distance <- toBoundedInteger <$> P.scientific
    endOfInput <|> endOfLine
    return ((min city1 city2, max city1 city2), distance)

getDistance :: Distances -> NodeState -> NodeState -> Distance
getDistance distances (city1, _) (city2, _) = distances Map.! (leftCity, rightCity)
    where
        leftCity = min city1 city2
        rightCity = max city1 city2

distancesParser :: Parser Distances
distancesParser = Map.fromList <$> many1' distanceParser

calculateNextCities :: Set.Set City -> NodeState -> Set.Set NodeState
calculateNextCities allCities (currentCity, visitedCities) = Set.map (\city -> (city, Set.insert city visitedCities)) $ allCities Set.\\ visitedCities

------------ PART A ------------
getAllCities :: Distances -> Set.Set City
getAllCities distances = Set.fromList . concatMap (\(city1, city2) -> [city1,city2]) $ Map.keys distances

getMinimumDistance :: Distances -> Int -> Distance
getMinimumDistance distances numberOfRemainingCities = sum . take (numberOfRemainingCities - 1) . sort . map snd $ Map.assocs distances

calculateRemainingCost :: Distances -> NodeState -> Int    
calculateRemainingCost distances (currentCity, cityVisited) = getMinimumDistance distances ((Set.size . getAllCities) distances - Set.size cityVisited)

getStartCity = head . Set.toList . getAllCities
getGoalCity = last . Set.toList . getAllCities

executeAStar distances allCities start = aStar (calculateNextCities allCities) (getDistance distances) (calculateRemainingCost distances) ((== allCities) . snd) (start, Set.singleton  start)

-- Part A:
-- Just 117
-- (0.007698s)
partA :: Distances -> Maybe Distance
partA = minimum . getAllShortestDistancesForVisitingAllCitiesOnce
    
getAllShortestDistancesForVisitingAllCitiesOnce distances = do
    let start = getStartCity distances
    let allCities = getAllCities distances
    Set.map (fmap fst . executeAStar distances allCities) allCities

------------ PART B ------------
-- Part B:
-- Just 909
-- (0.004028s)
partB = fmap abs . minimum . getAllLongestDistancesForVisitingAllCitiesOnce

getAllLongestDistancesForVisitingAllCitiesOnce distances = do
    let start = getStartCity distances
    let allCities = getAllCities distances
    Set.map (fmap fst . executeAStarPartB distances allCities) allCities

getMaximumDistance :: Distances -> Int -> Distance
getMaximumDistance distances numberOfRemainingCities = sum . take (numberOfRemainingCities - 1) . reverse . sort . map snd $ Map.assocs distances

calculateRemainingCostPartB :: Distances -> NodeState -> Int    
calculateRemainingCostPartB distances (currentCity, cityVisited) = (-1) * getMaximumDistance distances ((Set.size . getAllCities) distances - Set.size cityVisited)

executeAStarPartB distances allCities start = aStar (calculateNextCities allCities) (\c1 c2 -> (-1) * getDistance distances c1 c2) (calculateRemainingCostPartB distances) ((== allCities) . snd) (start, Set.singleton  start)