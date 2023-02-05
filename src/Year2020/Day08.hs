module Year2020.Day08 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
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
import Data.Text (Text, tail)
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
type InstructionPointer = Int
type Accumulator = Int

type ExecutionState = (InstructionPointer, [InstructionPointer], Accumulator) 

data Instruction = 
      ACC Int
    | NOP Int
    | JMP Int
    deriving(Eq, Show)

------------ PARSER ------------

inputParser :: Parser [Instruction]
inputParser = many instructionParser
    
instructionParser :: Parser Instruction
instructionParser = do
    instruction <- many letter
    " "
    Just value <- toBoundedInteger <$> P.scientific
    endOfInput <|> endOfLine
    return $
        case instruction of
            "acc" -> ACC value
            "nop" -> NOP value
            "jmp" -> JMP value

------------ PART A ------------

executeInstruction :: [Instruction] -> ExecutionState -> ExecutionState
executeInstruction instructions (ip, ipAlreadyExecuted, accumulator) = do
    case instructions !! ip of
        NOP _ -> (ip + 1, ip:ipAlreadyExecuted, accumulator)
        ACC value -> (ip + 1, ip:ipAlreadyExecuted, accumulator + value)
        JMP value -> (ip + value, ip:ipAlreadyExecuted, accumulator)

executeInstructionsUntilRepeat instructions = last . takeWhile (all ((== 1) . length) . group . sort . sel2) . iterate (executeInstruction instructions) $ (0, [], 0)

-- Part A:
-- 1420
-- (0.002328s)
partA = sel3 . executeInstructionsUntilRepeat

------------ PART B ------------
executeInstructionsUntilRepeatOrEnd instructions = until untilCondition (executeInstruction instructions) (0, [], 0)
    where 
        untilCondition executionState@(ip, ipAlreadyExecuted, _) = do
            let instructionRepeated = any ((/= 1) . length) . group . sort $ ipAlreadyExecuted
            let reachedEnd = ip >= length instructions
            instructionRepeated || reachedEnd

fixInstruction instructions at = do
    let (left, right) = splitAt at instructions
    case head right of
        ACC _ -> instructions
        JMP value -> left <> [NOP value] <> L.tail right
        NOP value -> left <> [JMP value] <> L.tail right

-- Part B:
-- 1245
-- (0.503071s)
partB instructions = sel3 . head . filter ((>= length instructions) . sel1) . map (executeInstructionsUntilRepeatOrEnd . fixInstruction instructions) $ [0..length instructions - 1]