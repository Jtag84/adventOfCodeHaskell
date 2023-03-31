module Year2019.IntCodeComputer where

import Data.Map.Strict qualified as Map
import Data.Attoparsec.Text as P (Parser, endOfInput, endOfLine, scientific, sepBy)
import Options.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Text.Printf (printf)

type Opcode = Int
type ProgramCounter = Int
type RelativeBase = Int
type IntCodeProgram = Map.Map ProgramCounter Opcode

type Inputs = [Int]
type Outputs = [Int]

data ProgramState = 
      Running ProgramCounter RelativeBase Inputs IntCodeProgram Outputs
    | Stopped ProgramCounter RelativeBase Inputs IntCodeProgram Outputs
    | NeedInput ProgramCounter RelativeBase Inputs IntCodeProgram Outputs
    deriving(Eq, Show, Ord)

startProgramState :: [Opcode] -> ProgramState
startProgramState opcodes = Running 0 0 [] (Map.fromList $ zip [0..] opcodes) []

isProgramNeedInput :: ProgramState -> Bool
isProgramNeedInput (Running {}) = False
isProgramNeedInput (NeedInput {}) = True
isProgramNeedInput (Stopped {}) = False

isProgramStopped :: ProgramState -> Bool
isProgramStopped (Running {}) = False
isProgramStopped (NeedInput {}) = False
isProgramStopped (Stopped {}) = True

setInputs :: Inputs -> ProgramState -> ProgramState
setInputs inputs (Running pc relativeBase _ opcodes outputs) = Running pc relativeBase inputs opcodes outputs
setInputs inputs (NeedInput pc relativeBase _ opcodes outputs) = Running pc relativeBase inputs opcodes outputs
setInputs _ (Stopped {}) = error "Program already stopped"

getOutputs :: ProgramState -> Outputs
getOutputs (Running _ _ _ _ outputs) = outputs
getOutputs (Stopped _ _ _ _ outputs) = outputs
getOutputs (NeedInput _ _ _ _ outputs) = outputs

clearOutputs :: ProgramState -> ProgramState
clearOutputs (Running pc relativeBase inputs opcodes _) = Running pc relativeBase inputs opcodes []
clearOutputs (NeedInput pc relativeBase inputs opcodes _) = NeedInput pc relativeBase inputs opcodes []
clearOutputs (Stopped pc relativeBase inputs opcodes _) = Stopped pc relativeBase inputs opcodes []

data ParameterMode = Immediate | Relative | Position
    deriving(Eq, Show)


opcodesParser :: Parser [Opcode]
opcodesParser = (fromJust . toBoundedInteger <$> scientific) `sepBy` "," <* (endOfLine <|> endOfInput)


executeOneStepIntCodeProgram :: ProgramState -> ProgramState
executeOneStepIntCodeProgram programSate@(Stopped {}) = error "already stopped"
executeOneStepIntCodeProgram programSate@(NeedInput pc relativeBase inputs opcodes outputs)
    | null inputs = error "needs input"
    | otherwise = executeOneStepIntCodeProgram $ Running pc relativeBase inputs opcodes outputs
executeOneStepIntCodeProgram (Running pc relativeBase inputs opcodes currentOutputs)
    -- end of program 
    | opcodes Map.! pc == 99 = Stopped pc relativeBase inputs opcodes currentOutputs

    -- addition
    | getFilteredOpcode == 1 = Running (pc+4) relativeBase inputs (executeOpcode (+) ) currentOutputs

    -- multiplication
    | getFilteredOpcode == 2 = Running (pc+4) relativeBase inputs (executeOpcode (*) ) currentOutputs

    -- store input
    | getFilteredOpcode == 3 && null inputs = NeedInput pc relativeBase inputs opcodes currentOutputs
    | getFilteredOpcode == 3 && (not . null) inputs = Running (pc+2) relativeBase (tail inputs) (Map.insert getFirstAddress (head inputs) opcodes) currentOutputs

    -- output value
    | getFilteredOpcode == 4 = Running (pc+2) relativeBase inputs opcodes $ currentOutputs ++ [getFirstValue]

    -- jump-if-true
    | getFilteredOpcode == 5 = 
        if getFirstValue /= 0
            then
                Running getSecondValue relativeBase inputs opcodes currentOutputs
            else
                Running (pc + 3) relativeBase inputs opcodes currentOutputs
    
    -- jump-if-false
    | getFilteredOpcode == 6 = 
        if getFirstValue == 0
            then
                Running getSecondValue relativeBase inputs opcodes currentOutputs
            else
                Running (pc + 3) relativeBase inputs opcodes currentOutputs
    
    -- less than
    | getFilteredOpcode == 7 = do
        let valueToStore = if getFirstValue < getSecondValue then 1 else 0
        Running (pc + 4) relativeBase inputs (Map.insert getThirdAddress valueToStore opcodes) currentOutputs 
    
    -- equals
    | getFilteredOpcode == 8 = do
        let valueToStore = if getFirstValue == getSecondValue then 1 else 0
        Running (pc + 4) relativeBase inputs (Map.insert getThirdAddress valueToStore opcodes) currentOutputs 
    
    -- set relative base
    | getFilteredOpcode == 9 = Running (pc + 2) (relativeBase + getFirstValue) inputs opcodes currentOutputs

    | otherwise = error $ "wrong opcode " ++ show getFilteredOpcode
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

-- >>> runIntCodeProgram [1] [109,1000,203,1,99]
-- Stopped 4 1000 [] (fromList [(0,109),(1,1000),(2,203),(3,1),(4,99),(1001,1)]) []
-- >>> runIntCodeProgram [1] [104,1125899906842624,99]
-- Stopped 2 0 [1] (fromList [(0,104),(1,1125899906842624),(2,99)]) [1125899906842624]
-- >>> runIntCodeProgram [1] [1102,34915192,34915192,7,4,7,99,0]
-- Stopped 6 0 [1] (fromList [(0,1102),(1,34915192),(2,34915192),(3,7),(4,4),(5,7),(6,99),(7,1219070632396864)]) [1219070632396864]
-- >>> runIntCodeProgram [1] [109,1,204,-1,1001,100,1,100,1008,100,16,101,99,1006,101,0,99]
-- Stopped 12 1 [1] (fromList [(0,109),(1,1),(2,204),(3,-1),(4,1001),(5,100),(6,1),(7,100),(8,1008),(9,100),(10,16),(11,101),(12,99),(13,1006),(14,101),(15,0),(16,99),(100,1),(101,0)]) [109]
-- >>> runIntCodeProgram [1] [109,1000,109,-3,1101,1,1,1000,203,3,204,3,99]
-- Stopped 12 997 [] (fromList [(0,109),(1,1000),(2,109),(3,-3),(4,1101),(5,1),(6,1),(7,1000),(8,203),(9,3),(10,204),(11,3),(12,99),(1000,1)]) [1]
runIntCodeProgram :: Inputs -> [Opcode] -> ProgramState
runIntCodeProgram inputs opcodes = until isProgramStopped executeOneStepIntCodeProgram (Running 0 0 inputs (Map.fromList $ zip [0..] opcodes) [])

runIntCodeProgramWithMemoryUpdate :: (Int, Int) -> Inputs -> [Opcode] -> ProgramState
runIntCodeProgramWithMemoryUpdate (memoryAddress, value) inputs opcodes = do
    let memory = Map.fromList $ zip [0..] opcodes
    let memoryUpdated = Map.alter (const . Just $ value) memoryAddress memory
    until isProgramStopped executeOneStepIntCodeProgram (Running 0 0 inputs memoryUpdated [])

executeUntilNOuputsOrStopped :: Int -> ProgramState -> ProgramState
executeUntilNOuputsOrStopped numberOfOutputs = until hasNOutputsOrStopped executeOneStepIntCodeProgram
    where
        hasNOutputsOrStopped programState = do
            let hasNOutputs = (==numberOfOutputs) . length . getOutputs $ programState
            let hasStopped = isProgramStopped programState
            hasStopped || hasNOutputs

executeUntilNeedsInputOrStopped :: ProgramState -> ProgramState
executeUntilNeedsInputOrStopped = until hasStoppedOrNeedsInput executeOneStepIntCodeProgram
    where
        hasStoppedOrNeedsInput programState = isProgramNeedInput programState || isProgramStopped programState
