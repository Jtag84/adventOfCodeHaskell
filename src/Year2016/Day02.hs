module Year2016.Day02 (runDay) where

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

inputParser :: Parser [[Direction]]
inputParser = many1' directionDigitParser

directionParser :: Parser Direction
directionParser = P.choice [
        char 'R' $> RightD,
        char 'L' $> LeftD,
        char 'U' $> UpD,
        char 'D' $> DownD
    ]

directionDigitParser :: Parser [Direction]
directionDigitParser = many1' directionParser <* (endOfInput <|> endOfLine)

------------ PART A ------------

coordinateToDigitMapPartA :: Map.Map Coordinate Char
coordinateToDigitMapPartA = Map.fromList [
        (XY (0,0),'1'),
        (XY (1,0),'2'),
        (XY (2,0),'3'),
        (XY (0,1),'4'),
        (XY (1,1),'5'),
        (XY (2,1),'6'),
        (XY (0,2),'7'),
        (XY (1,2),'8'),
        (XY (2,2),'9')
    ]

startFivePartA = XY (1,1)

toDigit = (Map.!) 

moveWithinBounds :: Set.Set Coordinate -> Coordinate -> Direction -> Coordinate
moveWithinBounds validCoordinateSet coordinate direction = do
    let newCoordinate = moveInDirection coordinate direction 1
    if newCoordinate `Set.member` validCoordinateSet
        then newCoordinate
        else coordinate

executeMovesForDigit validCoordinateSet = foldl' (moveWithinBounds validCoordinateSet) 

getAllDigitsCoordinates validCoordinateSet start = drop 1 . foldl' (\coordinates directions-> coordinates <>  [executeMovesForDigit validCoordinateSet (last coordinates) directions]) [start]

-- Part A:
-- "52981"
-- (0.000313s)
partA = map (toDigit coordinateToDigitMapPartA) . getAllDigitsCoordinates (Map.keysSet coordinateToDigitMapPartA) startFivePartA

------------ PART B ------------

coordinateToDigitMapPartB :: Map.Map Coordinate Char
coordinateToDigitMapPartB = Map.fromList [
        (XY (2,0),'1'),
        (XY (1,1),'2'),
        (XY (2,1),'3'),
        (XY (3,1),'4'),
        (XY (0,2),'5'),
        (XY (1,2),'6'),
        (XY (2,2),'7'),
        (XY (3,2),'8'),
        (XY (4,2),'9'),
        (XY (1,3),'A'),
        (XY (2,3),'B'),
        (XY (3,3),'C'),
        (XY (2,4),'D')
    ]

startFivePartB = XY (0,2)

-- Part B:
-- "74CD2"
-- (0.000681s)
partB = map (toDigit coordinateToDigitMapPartB) . getAllDigitsCoordinates (Map.keysSet coordinateToDigitMapPartB) startFivePartB