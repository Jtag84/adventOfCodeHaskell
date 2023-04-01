module Year2019.Day21 (runDay) where

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
import Year2019.IntCodeComputer (opcodesParser, runIntCodeProgram, getOutputs, Opcode)
import Util.Util (UnShow(UnShow))

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------


------------ PART A ------------
program =  "NOT A J\n" 
        ++ "NOT B T\n"
        ++ "OR T J\n"
        ++ "NOT C T\n"
        ++ "OR T J\n"
        ++ "AND D J\n"
        ++ "WALK\n"

programAscii = map fromEnum program

-- Part A:
-- 19348840
-- (0.120196s)
partA = last . getOutputs . runIntCodeProgram programAscii

------------ PART B ------------
programRun =   "NOT A J\n" 
            ++ "NOT B T\n"
            ++ "OR T J\n"
            ++ "NOT C T\n"
            ++ "OR T J\n"
            ++ "AND D J\n"
            ++ "NOT E T\n"
            ++ "AND E T\n"
            ++ "OR E T\n"
            ++ "OR H T\n"
            ++ "AND T J\n"
            ++ "RUN\n" 

programRunAscii = map fromEnum programRun

-- to visualize when it didn't make it accross:
-- UnShow . map toEnum . getOutputs . runIntCodeProgram programRunAscii
-- Part B:
-- 1141857182
-- (2.752836s)
partB = last . getOutputs . runIntCodeProgram programRunAscii