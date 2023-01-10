module Year2015.Day14 (runDay) where

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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

type Name = String
type Speed = Int
type FlyTyme = Int
type RestTime = Int
type Reindeer = (String, (Speed, FlyTyme, RestTime))

type Time = Int
type Distance = Int
------------ PARSER ------------

nameParser :: Parser String
nameParser = many1' letter

reindeerParser :: Parser Reindeer
reindeerParser = do
    name <- nameParser
    " can fly "
    Just speed <- toBoundedInteger <$> P.scientific
    " km/s for "
    Just flyTime <- toBoundedInteger <$> P.scientific
    " seconds, but then must rest for "
    Just restTime <- toBoundedInteger <$> P.scientific
    " seconds."
    endOfInput <|> endOfLine
    return (name, (speed, flyTime, restTime))

inputParser :: Parser [Reindeer]
inputParser = many1' reindeerParser

------------ PART A ------------

-- Part A:
-- 2655
-- (0.000024s)
partA = maximum . flip getDistancesAfterTime 2503

getDistancesAfterTime :: [Reindeer] -> Time -> [Distance]
getDistancesAfterTime reindeers numberOfSeconds = map (getDistanceAfterTime numberOfSeconds) reindeers

getDistanceAfterTime :: Time -> Reindeer -> Distance
getDistanceAfterTime numberOfSeconds (_, (speed, flyTime, restTime)) = do
    let (totalFlyTimeMultiplier, remainingTime) = numberOfSeconds `quotRem` (flyTime + restTime)
    speed * totalFlyTimeMultiplier * flyTime + min remainingTime flyTime * speed

------------ PART B ------------

-- Part B:
-- 1059
-- (0.004169s)
partB reindeers = maximum . foldr1 (zipWith (+)) $ map (getScores . getDistancesAfterTime reindeers) [1..2503]

getScores :: [Distance] -> [Int]
getScores distances = do
    let maxDistance = maximum distances
    map (addPointToMaxDistance maxDistance) distances
    where
        addPointToMaxDistance maxDistance distance = if distance == maxDistance then 1 else 0