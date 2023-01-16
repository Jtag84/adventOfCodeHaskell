module Year2019.Day02 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST, ST)
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
import Data.Array.ST (STArray)
import Data.Array as A (Array, (!), (//), listArray)
import Data.Tuple.All (Sel3(sel3))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Opcode = Int
type ProgramCounter = Int

------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = (fromJust . toBoundedInteger <$> scientific) `sepBy` "," <* (endOfLine <|> endOfInput)

------------ PART A ------------
executeOpcodes :: ProgramCounter -> Array ProgramCounter Opcode -> Array ProgramCounter Opcode
executeOpcodes pc opcodes 
    | opcodes A.! pc == 99 = opcodes
    | opcodes A.! pc == 1 = executeOpcodes (pc+4) $ executeOpcode (+) pc opcodes
    | opcodes A.! pc == 2 = executeOpcodes (pc+4) $ executeOpcode (*) pc opcodes
    | otherwise = error "wrong opcode"

executeOpcode operation pc opcodes = do
        let leftValue = opcodes A.! (opcodes A.! (pc + 1))
        let rightValue = opcodes A.! (opcodes A.! (pc + 2))
        let destinationIndex = opcodes A.! (pc + 3)
        let result = leftValue `operation` rightValue
        opcodes A.// [(destinationIndex, result)]

initializeInputs noun verb opcodes =  A.listArray (0,length opcodes -1) opcodes A.// [(1,noun), (2,verb)]

getResultForInputs noun verb = flip (A.!) 0 . executeOpcodes 0 . initializeInputs noun verb

-- Part A:
-- 6568671
-- (0.000032s)
partA = getResultForInputs 12 2

------------ PART B ------------

-- Part B:
-- 3951
-- (0.057091s)
partB opcodes = do
    let allPossibleInputs = [(noun, verb) | noun <- [0 .. 99], verb <- [0..99]]
    let (noun, verb, _) = head . take 1 . dropWhile ((/= 19690720) . sel3) . map (\(noun, verb) -> (noun, verb, getResultForInputs noun verb opcodes)) $ allPossibleInputs
    noun * 100 + verb