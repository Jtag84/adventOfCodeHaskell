module Year2019.Day09 (runDay) where

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
import Data.Array qualified as A
import Data.Bits qualified as Bit
import Text.Printf (printf)
import Data.Tuple.All (Sel4(sel4), Sel2 (sel2))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Opcode = Int
type ProgramCounter = Int
type RelativeBase = Int

type Inputs = [Int]
type Outputs = [Int]

data ParameterMode = Immediate | Relative | Position
    deriving(Eq, Show)
------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = (fromJust . toBoundedInteger <$> scientific) `sepBy` "," <* (endOfLine <|> endOfInput)

------------ PART A ------------

executeOpcodes :: ProgramCounter -> RelativeBase -> Inputs -> Map.Map ProgramCounter Opcode -> Outputs -> (Map.Map ProgramCounter Opcode, ProgramCounter, RelativeBase, Outputs)
executeOpcodes pc relativeBase inputs opcodes currentOutputs
    -- end of program 
    | opcodes Map.! pc == 99 = (opcodes, pc, relativeBase, currentOutputs)

    -- addition
    | getFilteredOpcode == 1 = executeOpcodes (pc+4) relativeBase inputs (executeOpcode (+) ) currentOutputs

    -- multiplication
    | getFilteredOpcode == 2 = executeOpcodes (pc+4) relativeBase inputs (executeOpcode (*) ) currentOutputs

    -- store input
    | getFilteredOpcode == 3 = executeOpcodes (pc+2) relativeBase (tail inputs) (Map.insert getFirstAddress (head inputs) opcodes) currentOutputs

    -- output value
    | getFilteredOpcode == 4 = executeOpcodes (pc+2) relativeBase inputs opcodes $ currentOutputs ++ [getFirstValue]

    -- jump-if-true
    | getFilteredOpcode == 5 = 
        if getFirstValue /= 0
            then
                executeOpcodes getSecondValue relativeBase inputs opcodes currentOutputs
            else
                executeOpcodes (pc + 3) relativeBase inputs opcodes currentOutputs
    
    -- jump-if-false
    | getFilteredOpcode == 6 = 
        if getFirstValue == 0
            then
                executeOpcodes getSecondValue relativeBase inputs opcodes currentOutputs
            else
                executeOpcodes (pc + 3) relativeBase inputs opcodes currentOutputs
    
    -- less than
    | getFilteredOpcode == 7 = do
        let valueToStore = if getFirstValue < getSecondValue then 1 else 0
        executeOpcodes (pc + 4) relativeBase inputs (Map.insert getThirdAddress valueToStore opcodes) currentOutputs 
    
    -- equals
    | getFilteredOpcode == 8 = do
        let valueToStore = if getFirstValue == getSecondValue then 1 else 0
        executeOpcodes (pc + 4) relativeBase inputs (Map.insert getThirdAddress valueToStore opcodes) currentOutputs 
    
    -- set relative base
    | getFilteredOpcode == 9 = executeOpcodes (pc + 2) (relativeBase + getFirstValue) inputs opcodes currentOutputs

    | otherwise = error "wrong opcode"
    where
        getFilteredOpcode = read [last $ show (opcodes Map.! pc)]
        executeOpcode operation = do
                let result = getFirstValue `operation` getSecondValue
                Map.insert getThirdAddress result opcodes

        getParameterMode paramNumber = getParameterModeFromInt (read [fullOpcodeString !! (3-paramNumber)] :: Int)
        fullOpcodeString = printf "%05d" (opcodes Map.! pc)

        getParameterValue paramNumber
            | getParameterMode paramNumber == Immediate = getAddress paramNumber
            | otherwise = Map.findWithDefault 0 (getAddress paramNumber) opcodes

        getParameter paramNumber = opcodes Map.! (pc + paramNumber)

        getAddress paramNumber
            | getParameterMode paramNumber == Relative = getParameter paramNumber + relativeBase
            | otherwise = getParameter paramNumber

        getFirstAddress = getAddress 1
        getThirdAddress = getAddress 3

        getFirstValue = getParameterValue 1
        getSecondValue = getParameterValue 2

        getParameterModeFromInt 0 = Position
        getParameterModeFromInt 1 = Immediate
        getParameterModeFromInt 2 = Relative

-- >>> runBoost 1 [109,1000,203,1,99]
-- (fromList [(0,109),(1,1000),(2,203),(3,1),(4,99),(1001,1)],4,1000,[])
-- >>> runBoost 1 [104,1125899906842624,99]
-- (fromList [(0,104),(1,1125899906842624),(2,99)],2,0,[1125899906842624])
-- >>> runBoost 1 [1102,34915192,34915192,7,4,7,99,0]
-- (fromList [(0,1102),(1,34915192),(2,34915192),(3,7),(4,4),(5,7),(6,99),(7,1219070632396864)],6,0,[1219070632396864])
-- >>> runBoost 1 [109,1,204,-1,1001,100,1,100,1008,100,16,101,99,1006,101,0,99]
-- (fromList [(0,109),(1,1),(2,204),(3,-1),(4,1001),(5,100),(6,1),(7,100),(8,1008),(9,100),(10,16),(11,101),(12,99),(13,1006),(14,101),(15,0),(16,99),(100,1),(101,0)],12,1,[109])
-- >>> runBoost 1 [109,1000,109,-3,1101,1,1,1000,203,3,204,3,99]
-- (fromList [(0,109),(1,1000),(2,109),(3,-3),(4,1101),(5,1),(6,1),(7,1000),(8,203),(9,3),(10,204),(11,3),(12,99),(1000,1)],12,997,[1])
runIntCodeProgram inputs opcodes = executeOpcodes 0 0 inputs (Map.fromList $ zip [0..] opcodes) [] 

------------ PART A ------------
-- Part A:
-- [3906448201]
-- (0.002342s)
partA  = sel4 . runIntCodeProgram [1]

------------ PART B ------------
-- Part B:
-- [59785]
-- (1.354287s)
partB = sel4 . runIntCodeProgram [2]
