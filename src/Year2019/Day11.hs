module Year2019.Day11 where

import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
import Data.Function (on)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger, normalize)
import Data.Sequence (mapWithIndex)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
    ( maxCol,
      maxRow,
      moveInDirection,
      rotate,
      toMatrixCoordinate,
      Coordinate(XY),
      Direction(UpD),
      normalizeCoordinates )
import Util.Util qualified as U
import Text.Printf (printf)
import Year2022.Day22 (moveForward)
import Data.Tuple.All (Sel3(sel3), Sel1 (sel1))
import Year2019.IntCodeComputer
    ( ProgramState(..),
      isProgramStopped,
      setInputs,
      getOutputs,
      clearOutputs,
      executeOneStepIntCodeProgram,
      opcodesParser, executeUntilNOuputsOrStopped )
import Util.Util (mapToPrettyPrintMatrix)

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------

type RobotState = (Coordinate, Direction)

data PanelColor = BlackPanel | WhitePanel
    deriving(Eq, Show, Enum)

type HullGrid = Map.Map Coordinate PanelColor

------------ PART A ------------

startRobotPosition = XY(0,0)
startRobotState = (startRobotPosition, UpD)

executeOneStepPaintAndMove :: (HullGrid, RobotState, ProgramState) -> (HullGrid, RobotState, ProgramState)
executeOneStepPaintAndMove (_, _, Stopped {}) = error "already stopped"
executeOneStepPaintAndMove (hullGrid, (robotPosition, robotDirection), programState@(Running {})) = do
    let newProgramState = executeUntilNOuputsOrStopped 2 (setInputs [fromEnum $ Map.findWithDefault BlackPanel robotPosition hullGrid] programState)
    let outputs@[colorToPaintThePanel, turnDirection] = getOutputs newProgramState
    let newHullGrid = Map.insert robotPosition (toEnum colorToPaintThePanel) hullGrid
    let newRobotDirection = rotate (toEnum turnDirection) robotDirection 
    let newRobotState = (moveInDirection robotPosition newRobotDirection 1, newRobotDirection)
    if isProgramStopped newProgramState
        then (hullGrid, (robotPosition, robotDirection), newProgramState)
        else (newHullGrid, newRobotState, clearOutputs newProgramState)

executeAllPaintingProgram startColor opcodes = until (isProgramStopped . sel3) executeOneStepPaintAndMove (Map.insert startRobotPosition startColor Map.empty, startRobotState, Running 0 0 [fromEnum WhitePanel] (Map.fromList $ zip [0..] opcodes) [])

-- Part A:
-- 1747
-- (0.315118s)
partA = Map.size . sel1 . executeAllPaintingProgram BlackPanel

------------ PART B ------------

-- Part B:
-- ┌                                                                                       ┐
-- │   ■ ■ ■ ■     ■ ■       ■ ■     ■ ■ ■     ■     ■   ■     ■   ■         ■ ■ ■         │
-- │         ■   ■     ■   ■     ■   ■     ■   ■     ■   ■   ■     ■         ■     ■       │
-- │       ■     ■         ■         ■     ■   ■ ■ ■ ■   ■ ■       ■         ■ ■ ■         │
-- │     ■       ■         ■   ■ ■   ■ ■ ■     ■     ■   ■   ■     ■         ■     ■       │
-- │   ■         ■     ■   ■     ■   ■   ■     ■     ■   ■   ■     ■         ■     ■       │
-- │   ■ ■ ■ ■     ■ ■       ■ ■ ■   ■     ■   ■     ■   ■     ■   ■ ■ ■ ■   ■ ■ ■         │
-- └                                                                                       ┘
-- ZCGRHKLB
-- (0.031932s)
partB program = mapToPrettyPrintMatrix (:[]) ' ' hullGridResultMap
    where
        hullGridResultMap = Map.fromList $ map (\(coordinates, color) -> (coordinates, if color == BlackPanel then ' ' else '■')) hullGridResult
        hullGridResult = Map.toList . sel1 
                        . executeAllPaintingProgram WhitePanel
                        $ program