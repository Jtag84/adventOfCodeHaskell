module Year2022.Day17 where

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
import qualified Control.Applicative.Combinators as P
import Util.Coordinate 
import Data.Foldable as Fold
import Data.Tuple.All (Sel3(sel3))
import Util.Util (takeUntilFirstNRepeats)

runDay :: R.Day
runDay = R.runDay directionsParser partA partB

------------ PARSER ------------
directionParser :: Parser Direction
directionParser = P.choice [
        char '<' $> LeftD,
        char '>' $> RightD
    ]

directionsParser :: Parser [Direction]
directionsParser = many1' directionParser

------------ TYPES ------------
type Rock = Vector Coordinate

horizontalLineRock :: Rock
horizontalLineRock = Vec.fromList [XY (0,0), XY (1,0), XY (2,0), XY (3,0)]

verticalLineRock :: Rock
verticalLineRock = Vec.fromList [XY (0,0), XY (0,1), XY (0,2), XY (0,3)]

plusRock :: Rock
plusRock = Vec.fromList [XY (1,0), XY (0,1), XY (1,1), XY (2,1), XY (1,2)]

lRock :: Rock
lRock = Vec.fromList [XY (0,0), XY (1,0), XY (2,0), XY (2,1), XY (2,2)]

cubeRock :: Rock
cubeRock = Vec.fromList [XY (0,0), XY (1,0), XY (0,1), XY (1,1)]

rockList :: [Rock]
rockList = cycle [horizontalLineRock, plusRock, lRock, verticalLineRock, cubeRock]

moveDown :: Rock -> Rock
moveDown = Vec.map (modifyY (+ (-1)))

moveRight :: Rock -> Rock
moveRight = Vec.map (modifyX (+1))

moveLeft :: Rock -> Rock
moveLeft = Vec.map (modifyX (+ (-1)))

-- >>> getRockHeight lRock
-- 3
-- >>> getRockHeight plusRock
-- 3
-- >>> getRockHeight verticalLineRock
-- 4
-- >>> getRockHeight horizontalLineRock
-- 1
getRockHeight :: Rock -> Int
getRockHeight rock = maxY rock - minY rock + 1

moveDirection :: Direction -> Rock -> Rock
moveDirection LeftD = moveLeft
moveDirection RightD = moveRight

------------ PART A ------------
fallingRock :: [Rock] -> [Direction] -> ([Rock], Set Coordinate, [Int]) -> ([Rock], Set Coordinate, [Int])
fallingRock (currentRock:nextRock:nextRocks) (currentDirection:nextDirections) fallenRocks@(stack, stackCoordinates, stackHeights)
    | length stack >= 2022 = fallenRocks
    | otherwise = do
        let nextPushedPosition = moveDirection currentDirection currentRock
        let rockPositionBeforeFalling = 
                if minX nextPushedPosition < 0 || maxX nextPushedPosition > 6 || isInStoppedRock nextPushedPosition
                    then
                        currentRock
                    else
                        nextPushedPosition
        
        let nextDownPosition = moveDown rockPositionBeforeFalling

        if isInStoppedRock nextDownPosition
            then
                do
                    let newStackCoordinates = Set.union (Set.fromList (Vec.toList rockPositionBeforeFalling)) stackCoordinates
                    let stackHeight = getStackHeight newStackCoordinates
                    fallingRock (initializeRockCoordinate stackHeight nextRock:nextRocks) nextDirections (rockPositionBeforeFalling:stack, newStackCoordinates, stackHeight:stackHeights)
            else
                fallingRock (nextDownPosition:nextRock:nextRocks) nextDirections fallenRocks
    where
        isInStoppedRock = Vec.any (`Set.member` stackCoordinates)

getStackHeight :: Set Coordinate -> Int
getStackHeight = maximum . Set.map getY

-- >>> initializeRockCoordinate 0 lRock
-- [XY (2,3),XY (3,3),XY (4,3),XY (4,4),XY (4,5)]
-- >>> initializeRockCoordinate 0 plusRock
-- [XY (3,3),XY (2,4),XY (3,4),XY (4,4),XY (3,5)]
-- >>> initializeRockCoordinate 0 cubeRock
-- [XY (2,3),XY (3,3),XY (2,4),XY (3,4)]
-- >>> initializeRockCoordinate 0 verticalLineRock
-- [XY (2,3),XY (2,4),XY (2,5),XY (2,6)]
-- >>> initializeRockCoordinate 0 horizontalLineRock
-- [XY (2,3),XY (3,3),XY (4,3),XY (5,3)]
initializeRockCoordinate :: Int -> Rock -> Rock
initializeRockCoordinate currentStackHeight = moveRight . moveRight . Vec.map (modifyY (+ (currentStackHeight + 4)))

-- Part A:
-- 3135
-- (1.347546s)
partA :: [Direction] -> Int
partA directions = do
    let (firstRock:next) = rockList
    (+1) . head . sel3 $ fallingRock (initializeRockCoordinate (-1) firstRock:next) (cycle directions) ([], Set.fromList (zipWith (curry XY) [0 .. 6] [-1, -1 ..]), [])

------------ PART B ------------
-- Part B:
-- 1569054441243
-- (1.375701s)
partB :: [Direction] -> Int
partB directions = getStackHeightForFallingRocks directions 1000000000000

getStackHeightForFallingRocks :: [Direction] -> Int -> Int
getStackHeightForFallingRocks directions numberOfFallingRocks = do
    let (firstRock:next) = rockList
    let heights = sel3 $ fallingRock (initializeRockCoordinate (-1) firstRock:next) (cycle directions) ([], Set.fromList (zipWith (curry XY) [0 .. 6] [-1, -1 ..]), [])
    let increaseHeights = zipWith (-) heights (tail heights) <> [1]
    let repeatableIncreaseHeights = reverse $ takeUntilFirstNRepeats 10 increaseHeights
    let repeatLength = length repeatableIncreaseHeights
    let repeatSum = sum repeatableIncreaseHeights
    let beforeRepeatableIncreaseHeights = U.takeUntil repeatableIncreaseHeights $ reverse increaseHeights
    let (whole, remainder) = quotRem (numberOfFallingRocks - length beforeRepeatableIncreaseHeights) repeatLength
    sum beforeRepeatableIncreaseHeights + whole * repeatSum + (sum . Data.List.take remainder) repeatableIncreaseHeights