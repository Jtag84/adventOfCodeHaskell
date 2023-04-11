module Year2019.Day25 (runDay) where

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
import Program.RunDay qualified as R (Day, runDay, debugDayWithPrompt)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import Year2019.IntCodeComputer (opcodesParser, executeUntilNeedsInputOrStopped, startProgramState, getOutputs, Opcode, setInputs, ProgramState, clearOutputs)
import Util.Util (UnShow(UnShow))
import Program.RunDay (Prompt)

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

-- To manually play the game
-- runDay = R.debugDayWithPrompt opcodesParser partA

------------ TYPES ------------

------------ PART A ------------


-- not the most efficient but gives the correct solution. Found by playing it manually
commands = map fromEnum . unlines $
                        [
                             "east"
                            ,"north"
                            ,"north"
                            ,"south"
                            ,"east"
                            ,"north"
                            ,"north"
                            ,"west"
                            ,"take asterisk"
                            ,"north"
                            ,"south"
                            ,"east"
                            ,"west"
                            ,"south"
                            ,"north"
                            ,"east"
                            ,"south"
                            ,"east"
                            ,"take sand"
                            ,"east"
                            ,"north"
                            ,"south"
                            ,"south"
                            ,"take tambourine"
                            ,"west"
                            ,"north"
                            ,"south"
                            ,"east"
                            ,"north"
                            ,"north"
                            ,"south"
                            ,"west"
                            ,"south"
                            ,"west"
                            ,"take prime number"
                            ,"east"
                            ,"north"
                            ,"east"
                            ,"south"
                            ,"west"
                            ,"north"
                            ,"west"
                        ]

runWithPrompt :: [Opcode] -> Prompt ProgramState
runWithPrompt opcodes = do
    let promptFunction state command = do
            let newState = executeUntilNeedsInputOrStopped . setInputs (map fromEnum (command ++ "\n")) $ clearOutputs state
            (map toEnum $ getOutputs newState, newState)
    let firstRun = executeUntilNeedsInputOrStopped $ startProgramState opcodes
    (map toEnum $ getOutputs firstRun, firstRun, promptFunction)


-- Found by playing the game by hand using runWithPrompt
-- Part A:
-- A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
-- Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
-- "Oh, hello! You should be able to get in by typing 2228740 on the keypad at the main airlock."
-- (0.748781s)
partA :: [Opcode] -> UnShow
partA = UnShow . map toEnum . getOutputs . executeUntilNeedsInputOrStopped . setInputs commands . startProgramState

-- To manually play the game:
-- partA :: [Opcode] -> Prompt ProgramState
-- partA = runWithPrompt

------------ PART B ------------

partB _ = "not done yet"