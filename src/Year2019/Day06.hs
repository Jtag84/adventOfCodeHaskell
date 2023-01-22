module Year2019.Day06 (runDay) where

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
import qualified Data.List as L

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type OrbitsAround = String
type Object = String
type Orbit = (OrbitsAround, [Object])

------------ PARSER ------------

inputParser :: Parser (Map.Map OrbitsAround [Object])
inputParser = Map.fromListWith (<>) <$> many1' orbitParser

orbitParser :: Parser Orbit
orbitParser = do
    orbitsAround <- many1' (letter <|> digit)
    ")"
    object <- many1' (letter <|> digit)
    endOfLine <|> endOfInput
    return (orbitsAround, [object])

------------ PART A ------------
calculateOrbits :: Map.Map OrbitsAround [Object] -> Int -> OrbitsAround -> Int
calculateOrbits orbitsMap currentCount orbitsAround = 
    currentCount + sum (map (calculateOrbits orbitsMap (currentCount +1)) (Map.findWithDefault [] orbitsAround orbitsMap))

-- Part A:
-- 224901
-- (0.000649s)
partA orbitsMap = calculateOrbits orbitsMap 0 "COM"

------------ PART B ------------

findPathTo :: Map.Map OrbitsAround [Object] -> Object -> [OrbitsAround] -> [OrbitsAround]
findPathTo orbitsMap obetctToFind currentPath = do
    let newObjects = Map.findWithDefault [] (last currentPath) orbitsMap
    if obetctToFind `elem` newObjects
        then 
            currentPath <> [obetctToFind]
        else
            concatMap (findPathTo orbitsMap obetctToFind . (currentPath <>) . (:[])) newObjects

-- Part B:
-- 334
-- (0.008798s)
partB orbitsMap =  do
    let pathToSanta = findPathTo orbitsMap "SAN" ["COM"]
    let pathToYou = findPathTo orbitsMap "YOU" ["COM"]
    let intersection = pathToSanta `L.intersect` pathToYou
    length pathToSanta + length pathToYou - 2 * length intersection - 2