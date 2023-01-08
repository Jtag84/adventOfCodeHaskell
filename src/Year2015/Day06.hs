{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Year2015.Day06 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText)
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
import Data.Bits (Bits(complement))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

data Instruction = 
    TurnOn (Coordinate, Coordinate)
    | TurnOff (Coordinate, Coordinate)
    | Toggle (Coordinate, Coordinate)
    deriving (Show, Eq)

type Lights = M.Matrix Bool
type LightsWithBrightness = M.Matrix Int
------------ PARSER ------------

inputParser :: Parser [Instruction]
inputParser = many1' instructionParser

coordinateParser :: Parser Coordinate
coordinateParser = do
    Just x <- toBoundedInteger <$> scientific
    ","
    Just y <- toBoundedInteger <$> scientific
    return $ XY (x,y)

fromToCoordinatesParser :: Parser (Coordinate,Coordinate)
fromToCoordinatesParser = do
    from <- coordinateParser
    " through "
    to <- coordinateParser
    endOfLine <|> endOfInput
    return (from, to)

instructionParser :: Parser Instruction
instructionParser = do
    P.choice [
            "turn on " >> TurnOn <$> fromToCoordinatesParser,
            "turn off " >> TurnOff <$> fromToCoordinatesParser,
            "toggle " >> Toggle <$> fromToCoordinatesParser
        ]
    
------------ PART A ------------
startLights :: Lights
startLights = M.matrix 1000 1000 (const False)

executeInstruction :: Lights -> Instruction -> Lights
executeInstruction lights (TurnOff range) = M.mapPos (applyActionIfWithinRange range (const False)) lights
executeInstruction lights (TurnOn range) = M.mapPos (applyActionIfWithinRange range (const True)) lights
executeInstruction lights (Toggle range) = M.mapPos (applyActionIfWithinRange range complement) lights

applyActionIfWithinRange :: (Coordinate, Coordinate) -> (a -> a) -> (Int, Int) -> a -> a
applyActionIfWithinRange range actionToApply currentPosition currentValue = 
    if isWithinRange range currentPosition
        then
            actionToApply currentValue
        else
            currentValue

isWithinRange (from, to) (currentRow, currentCol) = 
    getRow from <= currentRow 
    && getRow to >= currentRow 
    && getCol from <= currentCol 
    && getCol to >= currentCol

-- Part A:
-- 543903
-- (30.553759s)
partA = length . filter (== True) . M.toList . foldl' executeInstruction startLights

------------ PART B ------------

executeInstructionPartB :: LightsWithBrightness -> Instruction -> LightsWithBrightness
executeInstructionPartB lights (TurnOff range) = M.mapPos (applyActionIfWithinRange range decreaseBoundTo0) lights
executeInstructionPartB lights (TurnOn range) = M.mapPos (applyActionIfWithinRange range (+ 1)) lights
executeInstructionPartB lights (Toggle range) = M.mapPos (applyActionIfWithinRange range (+ 2)) lights

decreaseBoundTo0 0 = 0
decreaseBoundTo0 brightness = brightness - 1

startLightsWithBrightness :: LightsWithBrightness
startLightsWithBrightness = M.zero 1000 1000

-- Part B:
-- 14687245
-- (49.350010s)
partB = sum . M.toList . foldl' executeInstructionPartB startLightsWithBrightness