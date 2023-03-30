module Year2019.Day15 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState, MonadState (get))
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
import Year2019.IntCodeComputer (opcodesParser, ProgramState (Running), executeUntilNOuputsOrStopped, setInputs, getOutputs, clearOutputs, startProgramState)
import Util.Util (mapToPrettyPrintMatrix)
import Data.Bifunctor (Bifunctor(bimap))

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------
data LocationType = Wall | EmptyLT | Oxygen | Unknown | Robot
    deriving(Eq,Enum)

instance Show LocationType where
    show Wall = "■"
    show EmptyLT = " "
    show Oxygen = "O"
    show Unknown = "?"
    show Robot = "."

type RobotPosition = Coordinate

type RobotState = (RobotPosition, ProgramState)

type RobotCache = Cache RobotPosition (LocationType, ProgramState)

------------ PART A ------------
executeMove :: CardinalDirection -> ProgramState -> ProgramState
executeMove direction = executeUntilNOuputsOrStopped 1 . setInputs [fromCardinalDirection direction]

robotStartPosition = XY (0,0)

fromCardinalDirection North = 1
fromCardinalDirection South = 2
fromCardinalDirection West  = 3
fromCardinalDirection East  = 4

moveAndGetLocationType :: CardinalDirection -> RobotState -> (LocationType, ProgramState)
moveAndGetLocationType direction (robotPositon, programState) = do
    let programStateAfterMove = executeMove direction programState
    let newLocationType = toEnum . head . getOutputs $ programStateAfterMove
    (newLocationType, clearOutputs programStateAfterMove)

executeDijkstra :: (RobotState -> State RobotCache Bool) -> RobotState -> State RobotCache (Maybe (Int, [RobotState]))
executeDijkstra = dijkstraM cachedCalculateNeighbors getNeighborCost 
    where
        getNeighborCost a b = return 1
        cachedCalculateNeighbors robotState@(robotPosition, programState) = do
            northNeighbor <- getPositionAndRobotStateForNeighbor robotState North
            southNeighbor <- getPositionAndRobotStateForNeighbor robotState South
            eastNeighbor <- getPositionAndRobotStateForNeighbor robotState East
            westNeighbor <- getPositionAndRobotStateForNeighbor robotState West
            let validNeighbors = filter ((/= Wall) . fst . snd) [northNeighbor, southNeighbor, eastNeighbor, westNeighbor]
            return $ map (\(newRobotPosition, (_, newProgramSate)) -> (newRobotPosition, newProgramSate)) validNeighbors
        getPositionAndRobotStateForNeighbor robotState@(robotPosition, programState) direction = do
            let newPosition = moveInCardinalDirection robotPosition direction 1
            (newPosition,) <$> caching newPosition (const $ moveAndGetLocationType direction robotState)

findOxygenTank :: RobotState -> State RobotCache Bool
findOxygenTank (position, _) = do
        cache <- get
        return $ (fst <$> (cache Map.!? position)) == Just Oxygen

-- Part A:
-- (216,
-- ┌                                                                                 ┐
-- │ ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ? ■ ■ ■ ? │
-- │                                             . . . . . . . . . . . . . ■ . . . ■ │
-- │ ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ . ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ . ■ . ■ . ■ │
-- │ ? ? ? ? ? ? ? ? ? ? ? ■ . . . . . ■       ■ . ■   ■ . . . . . . . ■ . . . ■ O ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ ■ ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ■ . ■     . . . . . ■ . . . ■ . ■       ■ . . . . . . . ■ │
-- │ ? ? ? ? ? ? ? ? ? ? ? ■ . ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ . ■ . ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ . ■ │
-- │ ? ? ? ? ? ? ? ? ? ? ? ■ . . . . . ■     . . . ■ . . .             ■ . . . ■ . ■ │
-- │ ? ? ? ? ? ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ . ■ . ■ . ■ │
-- │ ? ? ? ? ? ■ . . . . . . . . . ■ . . . . . ■ . . . . . ■   ■ . . . . . ■ . . . ■ │
-- │ ? ? ? ? ■ ■ . ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■   ■ . ■ ■ ■ ■ ? ■ ■ ■ ? │
-- │ ? ? ? ■ . . . ■       ■ . . . ■ . . . . . ■       ■ . ■   ■ . ■ ? ? ? ? ? ? ? ? │
-- │ ? ? ? ■ . ■ ■ ■   ■   ■ . ■ ■ ■ . ■ ■ ■ ■ ■ ■ ■   ■ . ■ ■ ■ . ■ ? ? ? ? ? ? ? ? │
-- │ ? ? ? ■ . ■       ■     . . . . . ■               ■ . ■ . . . ■ ? ? ? ? ? ? ? ? │
-- │ ? ? ? ■ . ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ . ■ . ■ ■ ■ ■ ■ ■ ? ? ? ? ? │
-- │ ? ? ? ■ . . . . . . . ■ . . . . . ■         . . . . . ■ . . . . . . . ■ ? ? ? ? │
-- │ ? ? ? ? ■ ■ ■ ■ ■ ■ . ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ? ■ ■ ■ ■   ■ . ■ ■ ? ? ? │
-- │ ? ? ? ? ? ? ? ? ? ■ . . . ■ ? ■ . . . . . ■ . ■ . . . ■ . . . ■   ■ . . . ■ ? ? │
-- │ ? ? ? ? ? ? ? ? ? ? ■ ■ ■ ? ? ? ■ ■ ■ ■ . ■ . ■ . ■ . ■ . ■ . ■ ■ ■ ■ ■ . ■ ? ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ■ . ■ . . . ■ . . . ■ . . . . . . . ■ ? ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ■ . ■ ■ ■ ■ ? ■ ■ ■ ? ■ ■ ■ ■ ■ ■ ■ ? ? ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ■   ■ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? │
-- │ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ■ ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? │
-- └                                                                                 ┘)
-- (0.114643s)
partA opcodes = do
    let robotStartProgramState = startProgramState opcodes
    let (Just (shortestNumberOfMoveToOxygenTank, path), sectionMapWithProgramState) = runState (executeDijkstra findOxygenTank (robotStartPosition, robotStartProgramState)) (Map.singleton robotStartPosition (EmptyLT, robotStartProgramState))
    let sectionMap = Map.map fst sectionMapWithProgramState
    let sectionMapWithRobotPath = foldl (\currentMap pathPosistion -> Map.insert pathPosistion Robot currentMap) sectionMap (map fst $ init path)
    (shortestNumberOfMoveToOxygenTank, mapToPrettyPrintMatrix show Unknown sectionMapWithRobotPath)
    
------------ PART B ------------
findFurthestPositionFromOxygenTank :: RobotState -> State RobotCache Bool
findFurthestPositionFromOxygenTank (position, _) = do
        cache <- get
        let allEmptySpacePositions = map fst . filter ( (`elem` [EmptyLT, Oxygen]) . fst . snd) . Map.toList $ cache 
        return $ all ((== True) . isAllNeighborsExplored cache) allEmptySpacePositions
        where
            isNeighborExplored position cache direction = isJust $ cache Map.!? moveInCardinalDirection position direction 1
            isAllNeighborsExplored cache position = do
                let northNeighborAlreadyExplored = isNeighborExplored position cache North
                let southNeighborAlreadyExplored = isNeighborExplored position cache South
                let eastNeighborAlreadyExplored = isNeighborExplored position cache East
                let westNeighborAlreadyExplored = isNeighborExplored position cache West
                northNeighborAlreadyExplored && southNeighborAlreadyExplored && eastNeighborAlreadyExplored && westNeighborAlreadyExplored


-- Part B:
-- (326,
-- ┌                                                                                   ┐
-- │ ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ? ■ ■ ■ ? │
-- │ ■                                             . . . . . . . . . . . . . ■ . . . ■ │
-- │ ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ . ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ . ■ . ■ . ■ │
-- │ ■   ■               ■   ■ . . . . . ■       ■ . ■   ■ . . . . . . . ■ . . . ■ O ■ │
-- │ ■   ■ ■ ■   ■ ■ ■   ■   ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ ■ ? │
-- │ ■           ■   ■   ■   ■ . ■     . . . . . ■ . . . ■ . ■       ■ . . . . . . . ■ │
-- │ ? ■ ■ ■ ■ ■ ■   ■   ■   ■ . ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ . ■ . ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ . ■ │
-- │ ■       ■               ■ . . . . . ■     . . . ■ . . .             ■ . . . ■ . ■ │
-- │ ■   ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ . ■ . ■ . ■ │
-- │ ■   ■       ■ . . . . . . . . . ■ . . . . . ■ . . . . . ■   ■ . . . . . ■ . . . ■ │
-- │ ■   ■   ■ ■ ■ . ■ ■ ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ■ . ■   ■ . ■ ■ ■ ■ ■ ■ ■ ■ ? │
-- │ ■       ■ . . . ■       ■ . . . ■ . . . . . ■       ■ . ■   ■ . ■   ■           ■ │
-- │ ■   ■ ■ ■ . ■ ■ ■   ■   ■ . ■ ■ ■ . ■ ■ ■ ■ ■ ■ ■   ■ . ■ ■ ■ . ■   ■   ■   ■ ■ ? │
-- │ ■       ■ . ■       ■     . . . . . ■               ■ . ■ . . . ■       ■       ■ │
-- │ ? ■ ■   ■ . ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ . ■ . ■ ■ ■ ■ ■ ■ ■   ■   ■ │
-- │ ■   ■   ■ . . . . . . . ■ . . . . . ■         . . . . . ■ . . . . . . . ■   ■   ■ │
-- │ ■   ■   ■ ■ ■ ■ ■ ■ ■ . ■ . ■ ■ ■ . ■ ■ ■ ■ ■ . ■ ■ ■ ■ ? ■ ■ ■ ■   ■ . ■ ■ ■   ■ │
-- │ ■   ■       ■       ■ . . . ■   ■ . . . . . ■ . ■ . . . ■ . . . ■   ■ . . . ■   ■ │
-- │ ■   ■ ■ ■   ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ . ■ . ■ . ■ . ■ . ■ . ■ ■ ■ ■ ■ . ■   ■ │
-- │ ■   ■       ■       ■       ■           ■ . ■ . . . ■ . . . ■ . . . . . . . ■   ■ │
-- │ ■   ■   ■ ■ ■   ■   ■   ■   ■   ■ ■ ■   ■ . ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ │
-- │ ■       ■       ■   ■   ■       ■       ■   ■           ■       ■               ■ │
-- │ ■   ■ ■ ■   ■ ■ ■   ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■   ■   ■   ■   ■ ■ ■ ■ ■   ■ │
-- │ ■           ■   ■   ■           ■                   ■       ■   ■   ■       ■   ■ │
-- │ ? ■ ■ ■ ■ ■ ■   ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ ■ ■ ■ ■ ■ ■   ■   ■   ■ ■ ■   ■ │
-- │ ■               ■           ■           ■       ■           ■       ■   ■       ■ │
-- │ ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■   ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■   ■ ■ ■   ■   ■   ■ ■ ? │
-- │ ■   ■   ■       ■       ■       ■   ■   ■           ■   ■       ■       ■       ■ │
-- │ ■   ■   ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■   ■   ■ ■ ■ ■ ■   ■ ■ ■   ■ ■ ■ ■ ■ ■ ■   ■ │
-- │ ■   ■   ■           ■               ■   ■   ■               ■                   ■ │
-- │ ■   ■   ■ ■ ■ ■ ■   ■   ■ ■ ■ ■ ■   ■   ■   ■   ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ │
-- │ ■   ■                   ■       ■   ■   ■   ■                               ■   ■ │
-- │ ■   ■   ■ ■ ■ ■ ■   ■ ■ ■   ■   ■ ■ ■   ■   ■   ■ ■ ■ ■ ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■ │
-- │ ■   ■   ■       ■   ■       ■       ■       ■   ■               ■           ■   ■ │
-- │ ■   ■ ■ ■   ■   ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■   ■ ■ ■   ■ ■ ■ ■ ■   ■   ■ │
-- │ ■   ■       ■       ■           ■           ■           ■       ■       ■       ■ │
-- │ ■   ■   ■ ■ ? ■ ■   ■ ■ ■ ■ ■   ■   ■ ■ ■ ■ ? ■ ■ ■ ■ ■ ■   ■ ■ ■ ■ ■   ■ ■ ■ ■ ? │
-- │ ■   ■       ■   ■       ■       ■           ■               ■       ■           ■ │
-- │ ■   ■ ■ ■   ■   ■   ■ ■ ■   ■ ■ ■ ■ ■ ■ ■   ■   ■ ■ ■ ■ ■ ■ ■   ■   ■ ■ ■ ■ ■   ■ │
-- │ ■               ■                       ■                       ■               ■ │
-- │ ? ■ ■ ■ ■ ■ ■ ■ ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ? ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ? ■ ■ ■ ■ ■ ■ ■ ? │
-- └                                                                                   ┘)
-- (0.377802s)
partB opcodes = do
    let robotStartProgramState = startProgramState opcodes
    let (Just (_, robotPath), _) = runState (executeDijkstra findOxygenTank (robotStartPosition, robotStartProgramState)) (Map.singleton robotStartPosition (EmptyLT, robotStartProgramState))
    let oxygenTankPositionProgramState@(oxygenPosition, oxygenProgramState) = last robotPath
    let (Just (furthestPositionFromOxygen, oxygenPath), fullSectionMapWithProgramState) = runState (executeDijkstra findFurthestPositionFromOxygenTank oxygenTankPositionProgramState) (Map.singleton oxygenPosition (Oxygen, oxygenProgramState))
    let fullSectionMap = Map.map fst fullSectionMapWithProgramState

    let fullSectionMapWithRobotPath = foldl (\currentMap pathPosistion -> Map.insert pathPosistion Robot currentMap) fullSectionMap (map fst $ init robotPath)

    (furthestPositionFromOxygen, mapToPrettyPrintMatrix show Unknown fullSectionMapWithRobotPath)