module Year2016.Day01 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
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
import Util.Util (getFirstRepeating)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

type MoveForward = Int
type Instruction = (Rotation, MoveForward)
type Position = (Coordinate, CardinalDirection)

start :: Position
start = (XY(0,0), North)

------------ PARSER ------------

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy'` ", "

instructionParser :: Parser Instruction
instructionParser = do
    rotation <- P.choice [
            char 'L' $> CounterClockwise,
            char 'R' $> Clockwise
        ]
    Just numberForward <- toBoundedInteger <$> P.scientific
    return (rotation, numberForward)

------------ PART A ------------

-- Part A:
-- 279
-- (0.000015s)
partA = manhattanDistance (fst start) . fst . getFinalDestination start

getFinalDestination :: Position -> [Instruction] -> Position
getFinalDestination = foldl' applyInstruction 

applyInstruction :: Position -> Instruction -> Position
applyInstruction (coordinate, direction) (rotation, numberForward) = do
    let newDirection = rotateCardinal rotation direction
    let newCoordinate = moveInCardinalDirection coordinate newDirection numberForward
    (newCoordinate, newDirection)

------------ PART B ------------
-- >>> applyInstructionMovingOneAtATime start (Clockwise, 3)
-- [(XY (0,0),East),(XY (1,0),East),(XY (2,0),East)]
applyInstructionMovingOneAtATime :: Position -> Instruction -> [Position]
applyInstructionMovingOneAtATime (coordinate, direction) (rotation, numberForward) = do
    let newDirection = rotateCardinal rotation direction
    take numberForward . drop 1 $ iterate moveForwardOneAtATime (coordinate, newDirection)

moveForwardOneAtATime :: Position -> Position
moveForwardOneAtATime (coordinate, direction) = (moveInCardinalDirection coordinate direction 1, direction)

getAllIntermediaryPositions = foldl' (\positions instruction -> positions <> applyInstructionMovingOneAtATime (last positions) instruction) [start] 

-- Part B:
-- Just 163
-- (0.002308s)
partB = fmap (manhattanDistance (fst start)) . getFirstRepeating . map fst . getAllIntermediaryPositions