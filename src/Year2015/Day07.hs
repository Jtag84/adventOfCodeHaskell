{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Year2015.Day07 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, string)
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
import Util.Cache (Cache, caching, cachingState)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import qualified Data.Text as P
import Data.Bits 
import GHC.Word (Word16)

runDay :: R.Day
runDay = R.runDay circuitParser partA partB

------------ TYPES ------------
type Wire = String

data Signal = 
      Wire Wire
    | Value Word16
    deriving (Show, Eq)

data LogicGate = 
      Direct Signal
    | AND Signal Signal
    | OR Signal Signal
    | LSHIFT Signal Signal
    | RSHIFT Signal Signal
    | NOT Signal
    deriving (Show, Eq)

type Circuit = Map.Map Wire LogicGate

type CacheCircuit = Cache Wire Word16
------------ PARSER ------------

outputParser :: Parser Wire
outputParser = do
    " -> "
    wireName <- wireParser
    endOfLine <|> endOfInput
    return wireName

wireParser :: Parser Wire
wireParser = many1' letter

signalParser :: Parser Signal
signalParser = 
    P.choice [
            Wire <$> wireParser,
            Value . fromJust . toBoundedInteger <$>  scientific
        ]

twoSignalGateParser :: Text -> (Signal -> Signal -> LogicGate) ->  Parser LogicGate
twoSignalGateParser textToParse gateConstructor = do
    left <- signalParser
    P.string textToParse
    right <- signalParser
    return $ gateConstructor left right

oneSignalGateParser :: Text -> (Signal -> LogicGate) ->  Parser LogicGate
oneSignalGateParser textToParse gateConstructor = do
    P.string textToParse
    signal <- signalParser
    return $ gateConstructor signal

logicGateOutputParser :: Parser (Wire, LogicGate)
logicGateOutputParser = do
    logicGate <- P.choice [
            twoSignalGateParser " AND " AND,
            twoSignalGateParser " OR " OR,
            twoSignalGateParser " LSHIFT " LSHIFT,
            twoSignalGateParser " RSHIFT " RSHIFT,
            oneSignalGateParser "NOT " NOT,
            Direct <$> signalParser
        ]
    wireOutput <- outputParser
    return (wireOutput, logicGate)

circuitParser :: Parser Circuit
circuitParser = Map.fromList <$> many1' logicGateOutputParser

------------ PART A ------------

getWireValue :: Wire -> Circuit -> State CacheCircuit Word16
getWireValue wire circuit = getValueFromSignal circuit $ Wire wire

executeLogic :: Circuit -> LogicGate -> State CacheCircuit Word16
executeLogic circuit (Direct signal) = getValueFromSignal circuit signal
executeLogic circuit (AND leftSignal rightSignal) = (.&.) <$> getValueFromSignal circuit leftSignal <*> getValueFromSignal circuit rightSignal
executeLogic circuit (OR leftSignal rightSignal) =  (.|.) <$> getValueFromSignal circuit leftSignal <*> getValueFromSignal circuit rightSignal
executeLogic circuit (LSHIFT leftSignal rightSignal) = shiftL <$> getValueFromSignal circuit leftSignal <*> (fromEnum <$> getValueFromSignal circuit rightSignal)
executeLogic circuit (RSHIFT leftSignal rightSignal) = shiftR <$> getValueFromSignal circuit leftSignal <*> (fromEnum <$> getValueFromSignal circuit rightSignal)
executeLogic circuit (NOT signal) = complement <$> getValueFromSignal circuit signal

getValueFromSignal :: Circuit -> Signal -> State CacheCircuit Word16
getValueFromSignal circuit (Value value) = return value
getValueFromSignal circuit (Wire wire) = cachingState wire $ executeLogic circuit . (circuit Map.!)

-- Part A:
-- 16076
-- (0.000467s)
partA :: Circuit -> Word16
partA circuit = evalState (getWireValue "a" circuit) Map.empty

------------ PART B ------------

-- Part B:
-- 2797
-- (0.001241s)
partB :: Circuit -> Word16
partB circuit = do
    let aValue = evalState (getWireValue "a" circuit) Map.empty
    let newCircuit = Map.update (const $ Just (Direct (Value aValue))) "b" circuit
    evalState (getWireValue "a" newCircuit) Map.empty
