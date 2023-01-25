module Year2019.Day07 (runDay) where

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
import Year2019.Day05 qualified as Day5
import qualified Year2019.Day05 as Day5
import qualified Data.Array as A
import Text.Printf (printf)
import Data.Either (fromLeft, isLeft, fromRight, isRight)

runDay :: R.Day
runDay = R.runDay Day5.inputParser partA partB

type AmplifierControllerSoftware = [Day5.Opcode]

------------ PART A ------------
executeAmplifierControllerSoftware amplifierControllerSoftware inputs =  Day5.executeOpcodes 0 inputs (A.listArray (0,length amplifierControllerSoftware -1) amplifierControllerSoftware) [] 

runAmplifiersPartA :: AmplifierControllerSoftware -> Day5.Inputs -> Day5.Outputs
runAmplifiersPartA amplifierControllerSoftware inputs = do
    let ampA = executeAmplifierControllerSoftware amplifierControllerSoftware [head inputs, 0]
    let ampB = executeAmplifierControllerSoftware amplifierControllerSoftware [inputs !! 1, (head . snd) ampA]
    let ampC = executeAmplifierControllerSoftware amplifierControllerSoftware [inputs !! 2, (head . snd) ampB]
    let ampD = executeAmplifierControllerSoftware amplifierControllerSoftware [inputs !! 3, (head . snd) ampC]
    let ampE = executeAmplifierControllerSoftware amplifierControllerSoftware [inputs !! 4, (head . snd) ampD]
    snd ampE
    
partA amplifierControllerSoftware = maximum . map (runAmplifiersPartA amplifierControllerSoftware) . permutations $ [0,1,2,3,4]

------------ PART B ------------

type ProgramOpcodes = A.Array Day5.ProgramCounter Day5.Opcode
type ProgramState = (Day5.ProgramCounter, ProgramOpcodes)

executeOpcodesUntilNextOuptut :: ProgramState -> Day5.Inputs -> Day5.Outputs -> Either Day5.Outputs (ProgramState, Day5.Outputs)
executeOpcodesUntilNextOuptut (pc, opcodes) inputs currentOutputs
    | opcodes A.! pc == 99 = Left currentOutputs
    | getFilteredOpcode == 1 = executeOpcodesUntilNextOuptut (pc+4, executeOpcode (+)) inputs currentOutputs
    | getFilteredOpcode == 2 = executeOpcodesUntilNextOuptut (pc+4, executeOpcode (*)) inputs currentOutputs
    | getFilteredOpcode == 3 = executeOpcodesUntilNextOuptut (pc+2, opcodes A.// [(opcodes A.! (pc + 1), head inputs)]) (tail inputs) currentOutputs
    | getFilteredOpcode == 4 = Right ((pc+2, opcodes), getFirstValue : currentOutputs)
    | getFilteredOpcode == 5 = 
        if getFirstValue /= 0
            then
                executeOpcodesUntilNextOuptut (getSecondValue, opcodes) inputs currentOutputs
            else
                executeOpcodesUntilNextOuptut (pc + 3, opcodes) inputs currentOutputs
    | getFilteredOpcode == 6 = 
        if getFirstValue == 0
            then
                executeOpcodesUntilNextOuptut (getSecondValue, opcodes) inputs currentOutputs
            else
                executeOpcodesUntilNextOuptut (pc + 3, opcodes) inputs currentOutputs
    | getFilteredOpcode == 7 = do
        let valueToStore = if getFirstValue < getSecondValue then 1 else 0
        executeOpcodesUntilNextOuptut (pc + 4, opcodes A.// [(getThirdValue, valueToStore)]) inputs currentOutputs 
    | getFilteredOpcode == 8 = do
        let valueToStore = if getFirstValue == getSecondValue then 1 else 0
        executeOpcodesUntilNextOuptut (pc + 4, opcodes A.// [(getThirdValue, valueToStore)]) inputs currentOutputs 
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

type Output = Int
type Input = Int
type Phases = [Int]

initializeAmplifiers :: AmplifierControllerSoftware -> Phases -> Input -> ([ProgramState], Output)
initializeAmplifiers amplifierControllerSoftware phases input = do
    let initialProgramState = (0, A.listArray (0,length amplifierControllerSoftware -1) amplifierControllerSoftware)
    let Right (ampAState, ampAOutputs) = executeOpcodesUntilNextOuptut initialProgramState (head phases:[input]) []
    let Right (ampBState, ampBOutputs) = executeOpcodesUntilNextOuptut initialProgramState (phases !! 1:ampAOutputs) []
    let Right (ampCState, ampCOutputs) = executeOpcodesUntilNextOuptut initialProgramState (phases !! 2:ampBOutputs) []
    let Right (ampDState, ampDOutputs) = executeOpcodesUntilNextOuptut initialProgramState (phases !! 3:ampCOutputs) []
    let Right (ampEState, ampEOutputs) = executeOpcodesUntilNextOuptut initialProgramState (phases !! 4:ampDOutputs) []
    ([ampAState,ampBState,ampCState,ampDState,ampEState], head ampEOutputs)

runAmplifiersUntilNextOutput :: ([ProgramState], Input) -> Either Day5.Outputs ([ProgramState], Output)
runAmplifiersUntilNextOutput (programStates, input) = do
    (ampAState, ampAOutputs) <- executeOpcodesUntilNextOuptut (head programStates) [input] []
    (ampBState, ampBOutputs) <- executeOpcodesUntilNextOuptut (programStates !! 1) ampAOutputs []
    (ampCState, ampCOutputs) <- executeOpcodesUntilNextOuptut (programStates !! 2) ampBOutputs []
    (ampDState, ampDOutputs) <- executeOpcodesUntilNextOuptut (programStates !! 3) ampCOutputs []
    (ampEState, ampEOutputs) <- executeOpcodesUntilNextOuptut (programStates !! 4) ampDOutputs []
    Right ([ampAState,ampBState,ampCState,ampDState,ampEState], head ampEOutputs)

-- >>> runAmplifiersPartB [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5]
-- Right 139629729
runAmplifiersPartB amplifierControllerSoftware phases = do
    let init = initializeAmplifiers amplifierControllerSoftware phases 0 
    snd <$> (last . takeWhile isRight . iterate (runAmplifiersUntilNextOutput . fromRight (error "shouldn't happen")) $ Right init)

partB amplifierControllerSoftware = maximum . map (runAmplifiersPartB amplifierControllerSoftware) . permutations $ [5,6,7,8,9]