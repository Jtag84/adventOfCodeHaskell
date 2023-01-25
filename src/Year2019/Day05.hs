module Year2019.Day05 where

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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Opcode = Int
type ProgramCounter = Int

type Inputs = [Int]
type Outputs = [Int]
------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = (fromJust . toBoundedInteger <$> scientific) `sepBy` "," <* (endOfLine <|> endOfInput)

------------ PART A ------------

executeOpcodes :: ProgramCounter -> Inputs -> A.Array ProgramCounter Opcode -> Outputs -> (A.Array ProgramCounter Opcode, Outputs)
executeOpcodes pc inputs opcodes currentOutputs
    | opcodes A.! pc == 99 = (opcodes, currentOutputs)
    | getFilteredOpcode == 1 = executeOpcodes (pc+4) inputs (executeOpcode (+) ) currentOutputs
    | getFilteredOpcode == 2 = executeOpcodes (pc+4) inputs (executeOpcode (*) ) currentOutputs
    | getFilteredOpcode == 3 = executeOpcodes (pc+2) (tail inputs) (opcodes A.// [(opcodes A.! (pc + 1), head inputs)]) currentOutputs
    | getFilteredOpcode == 4 = executeOpcodes (pc+2) inputs opcodes $ getFirstValue : currentOutputs
    | getFilteredOpcode == 5 = 
        if getFirstValue /= 0
            then
                executeOpcodes getSecondValue inputs opcodes currentOutputs
            else
                executeOpcodes (pc + 3) inputs opcodes currentOutputs
    | getFilteredOpcode == 6 = 
        if getFirstValue == 0
            then
                executeOpcodes getSecondValue inputs opcodes currentOutputs
            else
                executeOpcodes (pc + 3) inputs opcodes currentOutputs
    | getFilteredOpcode == 7 = do
        let valueToStore = if getFirstValue < getSecondValue then 1 else 0
        executeOpcodes (pc + 4) inputs (opcodes A.// [(getThirdValue, valueToStore)]) currentOutputs 
    | getFilteredOpcode == 8 = do
        let valueToStore = if getFirstValue == getSecondValue then 1 else 0
        executeOpcodes (pc + 4) inputs (opcodes A.// [(getThirdValue, valueToStore)]) currentOutputs 
    | otherwise = error "wrong opcode"
    where
        getFilteredOpcode = read [last $ show (opcodes A.! pc)]
        executeOpcode operation = do
                let result = getFirstValue `operation` getSecondValue
                opcodes A.// [(getThirdValue, result)]

        isFirtParamValueImmediate = (read [fullOpcodeString !! 2] :: Int) == 1
        isSecondParamValueImmediate = (read [fullOpcodeString !! 1] :: Int) == 1
        fullOpcodeString = printf "%05d" (opcodes A.! pc)
        firstValueMode = if isFirtParamValueImmediate then id else (opcodes A.!)
        secondValueMode = if isSecondParamValueImmediate then id else (opcodes A.!)
        getFirstValue = firstValueMode (opcodes A.! (pc + 1))
        getSecondValue = secondValueMode (opcodes A.! (pc + 2))
        getThirdValue = opcodes A.! (pc + 3)

runDiagnostics systemId opcodes  = executeOpcodes 0 [systemId] (A.listArray (0,length opcodes -1) opcodes) [] 

-- Part A:
-- [16574641,0,0,0,0,0,0,0,0,0]
-- (0.000317s)
partA  = snd . runDiagnostics 1

------------ PART B ------------

-- Part B:
-- [15163975]
-- (0.000439s)
partB = snd . runDiagnostics 5