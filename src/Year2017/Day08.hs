module Year2017.Day08 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_, when)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState, MonadState (get), modify, gets, execState)
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
import Data.Tuple.All (Sel1(sel1), Sel2 (sel2), Upd1 (upd1), Upd2 (upd2), Upd3 (upd3), Upd4 (upd4), Sel3 (sel3), Sel4 (sel4))


runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

type RegisterState = (Int, Map.Map Register Int)

type Register = String

data Condition = Cond String (Int -> Bool)

instance Show Condition where
    show (Cond register _) = "comparing " ++ register

data Operation = Register `Inc` Int | Register `Dec` Int
    deriving(Eq, Show)

type Instruction = (Operation, Condition)

------------ PARSER ------------

inputParser :: Parser [Instruction]
inputParser = many instructionParser

registerParser :: Parser Register
registerParser = many letter

operationParser :: Parser Operation
operationParser = do
    registerToUpdate <- registerParser
    " "
    operation <- many letter
    " "
    Just value <- toBoundedInteger <$> scientific
    return $ case operation of
        "inc" -> registerToUpdate `Inc` value
        "dec" -> registerToUpdate `Dec` value

conditionParser :: Parser Condition
conditionParser = do
    registerToTest <- registerParser
    " "
    conditionOperand <- P.choice [
            ">=" $> (>=),
            ">" $> (>),
            "==" $> (==),
            "!=" $> (/=),
            "<=" $> (<=),
            "<" $> (<)
        ]
    " "
    Just value <- toBoundedInteger <$> scientific
    return $ Cond registerToTest (`conditionOperand` value)

instructionParser :: Parser Instruction
instructionParser = do
    operation <- operationParser
    " if "
    condition <- conditionParser
    endOfInput <|> endOfLine
    return (operation, condition)

------------ PART A ------------
getRegister :: Register -> State RegisterState Int
getRegister register = gets (Map.findWithDefault 0 register . snd)

modifyRegister :: Register -> (Int -> Int) -> State RegisterState ()
modifyRegister register valueFunction = do
    value <- getRegister register
    let newValue = valueFunction value
    modify (\(maxValue, registers) -> (max maxValue newValue, Map.alter (const $ Just newValue) register registers))

inc :: Register -> Int -> State RegisterState ()
inc register value = modifyRegister register (+ value)

dec :: Register -> Int -> State RegisterState ()
dec register value = modifyRegister register (+ (-value))

applyInstruction :: Instruction -> State RegisterState ()
applyInstruction (operation, Cond registerToTest test) = do
    condition <- test <$> getRegister registerToTest
    when condition $ applyOperation operation
    where 
        applyOperation (register `Inc` value) = register `inc` value
        applyOperation (register `Dec` value) = register `dec` value

applyAllInstructions :: [Instruction] -> State RegisterState [()]
applyAllInstructions = mapM applyInstruction

-- Part A:
-- ("nm",5849)
-- (0.001273s)
partA instructions = maximumBy (compare `on` snd) . Map.toList . snd . execState (applyAllInstructions instructions) $ (minBound, Map.empty)

------------ PART B ------------

-- Part B:
-- 6702
-- (0.000306s)
partB instructions = fst . execState (applyAllInstructions instructions) $ (minBound, Map.empty)