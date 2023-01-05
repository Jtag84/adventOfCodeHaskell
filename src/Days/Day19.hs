{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Days.Day19 where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Data.Scientific (toBoundedInteger)
import qualified Data.Attoparsec.Text as P
import Options.Applicative ((<|>), Alternative (empty), value)
import Control.Monad.State (State, evalState)
import Util.Cache
import Data.Tuple.All (Sel1(sel1), Sel2 (sel2), Sel3 (sel3), Sel4 (sel4))
import Algorithm.Search (aStarM, dijkstraM)

{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay blueprintsParser partA partB

------------ PARSER ------------

blueprintParser :: Parser BluePrint
blueprintParser = do
    "Blueprint "
    scientific
    ": Each ore robot costs "
    Just oreRobotOreCost <- toBoundedInteger <$> scientific
    " ore. Each clay robot costs "
    Just clayRobotOreCost <- toBoundedInteger <$> scientific
    " ore. Each obsidian robot costs "
    Just obsidianRobotOreCost <- toBoundedInteger <$> scientific
    " ore and "
    Just obsidianRobotClayCost <- toBoundedInteger <$> scientific
    " clay. Each geode robot costs "
    Just geodeRobotOreCost <- toBoundedInteger <$> scientific
    " ore and "
    Just geodeRobotObsidianCost <- toBoundedInteger <$> scientific
    " obsidian."
    endOfLine <|> endOfInput
    return ((oreRobotOreCost,0,0,0),(clayRobotOreCost,0,0,0),(obsidianRobotOreCost,obsidianRobotClayCost,0,0),(geodeRobotOreCost,0,geodeRobotObsidianCost,0))

blueprintsParser :: Parser [BluePrint]
blueprintsParser = many' blueprintParser <* endOfInput


------------ TYPES ------------

type Ore = Int
type Clay = Int
type Obsidian = Int
type Geode = Int
type Materials = (Ore,Clay,Obsidian,Geode)

type RobotBluePrint = Materials

type OreRobot = RobotBluePrint
type ClayRobot = RobotBluePrint
type ObsidianRobot = RobotBluePrint
type GeodeRobot = RobotBluePrint

type Inventory = Materials
type Robots = (Ore,Clay,Obsidian,Geode)

type BluePrint = (OreRobot,ClayRobot,ObsidianRobot,GeodeRobot)

type TimeStep = Int
type NodeState = (TimeStep, Inventory, Robots)

type CacheTimeStepInventory = Cache NodeState (Set NodeState)

getOre :: Materials -> Int
getOre = sel1

getClay :: Materials -> Int
getClay = sel2 

getObsidian :: Materials -> Int
getObsidian = sel3

getGeode :: Materials -> Int
getGeode = sel4

getOreRobot :: BluePrint -> RobotBluePrint
getOreRobot = sel1

getClayRobot :: BluePrint -> RobotBluePrint
getClayRobot = sel2 

getObsidianRobot :: BluePrint -> RobotBluePrint
getObsidianRobot = sel3

getGeodeRobot :: BluePrint -> RobotBluePrint
getGeodeRobot = sel4

robotToAdd = ((1,0,0,0), (0,1,0,0), (0,0,1,0), (0,0,0,1))

toGetMaterial :: (BluePrint -> RobotBluePrint) -> (Materials -> Int)
toGetMaterial getRobot = case getRobot robotToAdd of
                            (1,0,0,0) -> getOre
                            (0,1,0,0) -> getClay
                            (0,0,1,0) -> getObsidian
                            (0,0,0,1) -> getGeode

cachedCalculateNeighbors :: BluePrint -> NodeState -> State CacheTimeStepInventory (Set NodeState)
cachedCalculateNeighbors blueprint currentNodeState = caching currentNodeState $ calculateNeighbors blueprint

calculateNeighbors :: BluePrint -> NodeState -> Set NodeState
calculateNeighbors blueprint currentNodeState@(currentTimeStep, currentInventory, currentRobots) = do
    Set.fromList $ buildRobot getOreRobot <> buildRobot getClayRobot <> buildRobot getObsidianRobot <> buildRobot getGeodeRobot <> [(nextTimestep, nextInventory, currentRobots)]
    where
        buildRobot :: (BluePrint -> RobotBluePrint) -> [NodeState]
        buildRobot getRobotBluePrint = do
            let robotBlueprint = getRobotBluePrint blueprint
            if currentInventory `hasEnoughMaterialFor` robotBlueprint && couldntBuildBefore robotBlueprint && not (maxBuildHasBeenReached (toGetMaterial getRobotBluePrint))
                then
                    [(nextTimestep, nextInventory `sub` robotBlueprint, currentRobots `add` getRobotBluePrint robotToAdd)]
                else
                    []

        nextTimestep = currentTimeStep + 1
        nextInventory = currentInventory `add` currentRobots
        couldntBuildBefore robotBlueprint = not $ (currentInventory `sub` currentRobots) `hasEnoughMaterialFor` robotBlueprint
        
        maxBuildHasBeenReached getRobot = getRobot currentRobots >= getRobot maxRobots 
        
        maxRobots = (maxOreRobots, maxClayRobots, maxObsidian, maxBound :: Int)

        maxOreRobots = maxMaterial getOre
        maxClayRobots = maxMaterial getClay
        maxObsidian = maxMaterial getObsidian
        
        maxMaterial :: (Materials -> Int) -> Int
        maxMaterial getMaterial = maximum $ map (flip (getMaterial .) blueprint) [getOreRobot, getClayRobot, getObsidianRobot, getGeodeRobot]

getNeighborCost :: NodeState -> NodeState -> State CacheTimeStepInventory Int
getNeighborCost (_, (fromOre,fromClay,fromObsidian,fromGeodes), (_,_,_,fromRobotGeode)) (currentTime, (toOre,toClay,toObsidian,toGeodes), (_,_,_,toRobotGeode)) = return $ 100 - fromGeodes - toGeodes - fromRobotGeode - toRobotGeode

heuristic :: Goal -> NodeState -> State CacheTimeStepInventory Int
heuristic goal (currentTime, (_,_,_,currentGeodes), _) = return $ goal - currentTime - currentGeodes 

isGoal :: Goal -> NodeState -> State CacheTimeStepInventory Bool
isGoal goal (currentTime, (_,_,_,_), _) = return $ goal == currentTime

type Start = TimeStep
type Goal = TimeStep

-- executeAStar start goal blueprint = aStarM (cachedCalculateNeighbors blueprint) getNeighborCost (heuristic goal) (isGoal goal) (start, (0,0,0,0), (1,0,0,0))

executeDijkstra :: Start -> Goal -> BluePrint -> State CacheTimeStepInventory (Maybe (Int, [NodeState]))
executeDijkstra start goal blueprint = dijkstraM (cachedCalculateNeighbors blueprint) getNeighborCost (isGoal goal) (start, (0,0,0,0), (1,0,0,0))

-- >>> (1,2,3,4) `add` (1,1,-1,4)
-- (2,3,2,8)
add :: Materials -> Materials -> Materials
add (leftOre,leftClay,leftObsidian,leftGeode) (rightOre,rightClay,rightObsidian,rightGeode) = (leftOre + rightOre,leftClay + rightClay,leftObsidian + rightObsidian,leftGeode + rightGeode)

-- >>> (1,2,3,4) `sub` (1,1,-1,4)
-- (0,1,4,0)
sub :: Materials -> Materials -> Materials
sub (leftOre,leftClay,leftObsidian,leftGeode) (rightOre,rightClay,rightObsidian,rightGeode) = (leftOre - rightOre, leftClay - rightClay, leftObsidian - rightObsidian, leftGeode - rightGeode)

-- >>> (1,2,3,4) `hasEnoughMaterialFor` (1,1,0,4)
-- True
-- >>> (1,2,3,4) `hasEnoughMaterialFor` (1,3,0,0)
-- False
hasEnoughMaterialFor :: Inventory -> RobotBluePrint -> Bool
hasEnoughMaterialFor (inventoryOre,inventoryClay,inventoryObsidian,inventoryGeode) (robotBluePrintOre,robotBluePrintClay,robotBluePrintObsidian,robotBluePrintGeode) =
    inventoryOre >= robotBluePrintOre && inventoryClay >= robotBluePrintClay && inventoryObsidian >= robotBluePrintObsidian && inventoryGeode >= robotBluePrintGeode

------------ PART A ------------
calculateQualityLevel :: (Int, Maybe (Int, [NodeState])) -> Int
calculateQualityLevel (index, Just (_, nodes)) = index * getGeodesProduced nodes

getInventory = sel2

getGeodesProduced = getGeode . getInventory . last

-- Part A:
-- 1023
-- (11.190902s)
partA :: [BluePrint] -> Int
partA blueprints = sum $ zipWith (curry calculateQualityLevel) [1..] $ map (\blueprint -> evalState (executeDijkstra 0 24 blueprint) Map.empty) blueprints

-- Part B:
-- [26,51,10]
-- (60.979740s)
-- there is an unresolved bug where the last robot geode isn't taken into account so the real answer is [26,52,10]
-- >>> 26*52*10
-- 13520
partB blueprints = map (\blueprint -> evalState (executeDijkstra 0 32 blueprint) Map.empty) $ Data.List.take 3 blueprints
