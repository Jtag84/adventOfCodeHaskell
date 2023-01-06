module Year2022.Day24 (runDay) where

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
import Util.Coordinate
import Util.Range (Range (Range), isInRange)
import Control.Monad.State (State, evalState, runState)
import Util.Cache (Cache, caching)
import GHC.Exts (currentCallStack)
import Algorithm.Search (aStarM)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay blizzardParser partA partB

------------ PARSER ------------
blizzardParser  :: Parser (BottomRightBoundary, Set Blizzard)
blizzardParser = do
    snd <$> runScanner (XY (0,0) ,Set.empty) scanPredicate 
    where
        scanPredicate :: (Coordinate, Set Blizzard) -> Char -> Maybe (Coordinate, Set Blizzard)
        scanPredicate (currentCoordinate, blizzards) currentChar = 
            case currentChar of
                '>'  -> Just (nextOnLine currentCoordinate, Set.insert (currentCoordinate, RightD) blizzards)
                '<'  -> Just (nextOnLine currentCoordinate, Set.insert (currentCoordinate, LeftD) blizzards)
                '^'  -> Just (nextOnLine currentCoordinate, Set.insert (currentCoordinate, UpD) blizzards)
                'v'  -> Just (nextOnLine currentCoordinate, Set.insert (currentCoordinate, DownD) blizzards)
                '.'  -> Just (nextOnLine currentCoordinate, blizzards)
                '#'  -> Just (nextOnLine currentCoordinate, blizzards)
                '\n' -> Just (nextLine currentCoordinate, blizzards)
                _    -> Nothing

        nextOnLine (XY (x, y)) = XY (x+1, y)
        nextLine (XY (x, y)) = XY (0, y+1)

------------ TYPES ------------
type BottomRightBoundary = Coordinate

type Blizzard = (Coordinate, Direction)

type NextBlizzardCache = Cache (Set Blizzard) (Set Blizzard)

type NodeState = (Coordinate, Set Blizzard)

------------ PART A ------------

getMinXBoundary :: Int
getMinXBoundary = 1

getMaxXBoundary :: BottomRightBoundary -> Int
getMaxXBoundary (XY (x,_)) = x - 2

getMinYBoundary :: Int
getMinYBoundary = 1

getMaxYBoundary :: BottomRightBoundary -> Int
getMaxYBoundary (XY (_,y)) = y - 1

getStart :: Coordinate
getStart = XY (1,0)

getGoal :: BottomRightBoundary -> Coordinate
getGoal (XY (x,y)) = XY (x-2, y)

getNextBlizzards :: BottomRightBoundary -> Set Blizzard -> State NextBlizzardCache (Set Blizzard)
getNextBlizzards boundary blizzards = do
    caching blizzards $ Set.map getNextBlizzard
    where
        getNextBlizzard (XY (x,y), RightD) =  (wrapX (XY (x+1, y)), RightD)
        getNextBlizzard (XY (x,y), LeftD) =  (wrapX (XY (x-1, y)), LeftD)
        getNextBlizzard (XY (x,y), UpD) =  (wrapY (XY (x, y - 1)), UpD)
        getNextBlizzard (XY (x,y), DownD) =  (wrapY (XY (x, y + 1)), DownD)
        wrapX coordinate@(XY (x, y))
            | x < getMinXBoundary = XY (maxX, y)
            | x > maxX = XY (getMinXBoundary, y)
            | otherwise = coordinate
        wrapY coordinate@(XY (x, y))
            | y < getMinYBoundary = XY (x, maxY)
            | y > maxY = XY (x, getMinYBoundary)
            | otherwise = coordinate
        maxX = getMaxXBoundary boundary
        maxY = getMaxYBoundary boundary

calculateNeighbors :: BottomRightBoundary -> NodeState -> State NextBlizzardCache (Set NodeState)
calculateNeighbors bottomRightBoundary (currentCoordinate, currentBlizzards) = do
    nextBlizzards <- getNextBlizzards bottomRightBoundary currentBlizzards
    let nexBlizzardCoordinates = Set.map fst nextBlizzards
    let neighbors = Set.fromList 
            $ filter isWithinBoundaries
            $ filter (`Set.notMember` nexBlizzardCoordinates) 
            $ [getNorth, getSouth, getEast, getWest, id] <*> pure currentCoordinate
    return $ Set.map (,nextBlizzards) neighbors
    where
        isWithinBoundaries coordinate
            | getX coordinate >= getMinXBoundary && getX coordinate <= maxX  && getY coordinate >= getMinYBoundary && getY coordinate <= maxY = True
            | coordinate == getGoal bottomRightBoundary = True
            | coordinate == getStart = True
            | otherwise = False
        maxX = getMaxXBoundary bottomRightBoundary
        maxY = getMaxYBoundary bottomRightBoundary
    
getNeighborCost :: NodeState -> NodeState -> State NextBlizzardCache Int
getNeighborCost from to = return 1

manhattanDistanceBetweenNodeState :: Coordinate -> NodeState -> State NextBlizzardCache Int
manhattanDistanceBetweenNodeState from (to, _) = return $ manhattanDistance from to

isGoal :: Coordinate -> NodeState -> State NextBlizzardCache Bool
isGoal goal (coordinate, _) = return $ goal == coordinate

type Start = Coordinate
type Goal = Coordinate
executeAStar :: (BottomRightBoundary, Set Blizzard) -> Start -> Goal -> State NextBlizzardCache (Maybe (Int, [NodeState]))
executeAStar (bottomRightBoundary, blizzards) start goal = aStarM (calculateNeighbors bottomRightBoundary) getNeighborCost (manhattanDistanceBetweenNodeState goal) (isGoal goal) (start, blizzards)

-- Part A:
-- Just 311
-- (67.755074s)
partA :: (BottomRightBoundary, Set Blizzard) -> Maybe Int
partA bottomRightBoundaryAndBlizzards@(bottomRightBoundary, _) = fst <$> evalState (executeAStar bottomRightBoundaryAndBlizzards getStart goal) Map.empty
    where
        goal = getGoal bottomRightBoundary

------------ PART B ------------

-- Part B:
-- Just 869
-- (165.349373s)
partB :: (BottomRightBoundary, Set Blizzard) -> Maybe Int
partB bottomRightBoundaryAndBlizzards@(bottomRightBoundary, _) = do
    let (Just (startToGoalTime, startToGoalNodes), startToGoalState) = runState (executeAStar bottomRightBoundaryAndBlizzards start goal) Map.empty
    let (Just (goBackToGoalTime, goBackToGoalNodes), goBackToGoalState) = runState (executeAStar (getBottomRightBoundaryAndBlizzardsFromPrevious startToGoalNodes) goal start) startToGoalState
    let (backAgainToGoalTime, _) = runState (executeAStar (getBottomRightBoundaryAndBlizzardsFromPrevious goBackToGoalNodes) start goal) goBackToGoalState
    (startToGoalTime + goBackToGoalTime + ). fst <$> backAgainToGoalTime 
    where
        start = getStart
        goal = getGoal bottomRightBoundary
        getBottomRightBoundaryAndBlizzardsFromPrevious nodes = (bottomRightBoundary, (snd . last) nodes)