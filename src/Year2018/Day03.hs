module Year2018.Day03 (runDay) where

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
import Util.Util (getAllPairs)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type ClaimId = Int
type Claim = (ClaimId, Set.Set Coordinate)

------------ PARSER ------------

inputParser :: Parser [Claim]
inputParser = many1' claimParser

claimParser :: Parser Claim
claimParser = do
    claimId <- claimIdParser
    " @ "
    coordinates <- claimCoordinatesParser
    endOfInput <|> endOfLine
    return (claimId, coordinates)

claimIdParser :: Parser ClaimId
claimIdParser = char '#' *> (fromJust . toBoundedInteger <$> scientific)

claimCoordinatesParser :: Parser (Set.Set Coordinate)
claimCoordinatesParser = do
    Just xOffset <- toBoundedInteger <$> scientific
    ","
    Just yOffset <- toBoundedInteger <$> scientific
    ": "
    Just width <- toBoundedInteger <$> scientific
    "x"
    Just height <- toBoundedInteger <$> scientific
    return $ Set.fromList [XY (x,y) | x <- [xOffset .. xOffset + width - 1], y <- [yOffset .. yOffset+height - 1]]

------------ PART A ------------

findOverlappingCoordinates ((_,claim1Coordinates), (_,claim2Coordinates)) = Set.intersection claim1Coordinates claim2Coordinates

findAllOverlappingCoordinates = Set.unions . map findOverlappingCoordinates . getAllPairs 

-- Part A:
-- 116489
-- (1.649099s)
partA :: [(ClaimId, Set.Set Coordinate)] -> Int
partA = Set.size . findAllOverlappingCoordinates

------------ PART B ------------

-- Part B:
-- [1260]
-- (1.625788s)
partB claims = do
    let overlappingCoordinates = findAllOverlappingCoordinates claims
    map fst $ filter (all (`Set.notMember` overlappingCoordinates) . snd) claims