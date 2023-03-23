module Year2019.Day10 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, runScanner, scientific, takeText, letter, scientific)
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
import Data.Ratio

runDay :: R.Day
runDay = R.runDay asteroidParser partA partB

------------ TYPES ------------
type Asteroid = Coordinate

------------ PARSER ------------
asteroidParser  :: Parser (Set.Set Asteroid)
asteroidParser = do
    Set.fromList . snd . snd <$> runScanner (XY (0,0) ,[]) scanPredicate 
    where
        scanPredicate :: (Coordinate, [Asteroid]) -> Char -> Maybe (Coordinate, [Asteroid])
        scanPredicate (currentCoordinate, asteroids) currentChar = 
            case currentChar of
                '#'  -> Just (nextOnLine currentCoordinate, currentCoordinate:asteroids)
                '.'  -> Just (nextOnLine currentCoordinate, asteroids)
                '\n' -> Just (nextLine currentCoordinate, asteroids)
                _    -> Nothing

        nextOnLine (XY (x, y)) = XY (x+1, y)
        nextLine (XY (x, y)) = XY (0, y+1)

------------ PART A ------------

-- >>> calculateBlockinCoordinates (XY(1,1)) (XY(22,40))
-- fromList [XY (8,14),XY (15,27)]
-- >>> calculateBlockinCoordinates (XY(1,0)) (XY(4,3))
-- fromList [XY (2,1),XY (3,2)]
-- >>> calculateBlockinCoordinates (XY(1,0)) (XY(4,4))
-- fromList []
-- >>> calculateBlockinCoordinates (XY(1,0)) (XY(3,4))
-- fromList [XY (2,2)]
-- >>> calculateBlockinCoordinates (XY(1,0)) (XY(3,4))
-- fromList [XY (2,2)]
-- >>> calculateBlockinCoordinates (XY(4,0)) (XY(4,4))
-- fromList [XY (4,1),XY (4,2),XY (4,3)]
calculateBlockinCoordinates :: Asteroid -> Asteroid -> Set.Set Coordinate
calculateBlockinCoordinates from to = do
    let exactCoordinates =
            if xTo == xFrom
                then [(xTo,fromIntegral y) | y <- [minY [from,to] .. maxY [from,to]]]
                else [(x,y) | x <- [minX [from,to] .. maxX [from,to]], y <- [lineFunctionGetY x]]
    let lineCoordinate = Set.fromList $ map (\(x,y) -> XY(x, round y)) . filter (isInt . snd) $ exactCoordinates
    lineCoordinate `Set.difference` Set.fromList [from, to]
    where
        xTo = getX to
        yTo = getY to
        xFrom = getX from
        yFrom = getY from
        lineFunctionGetY = do
            let slope  = (yTo - yFrom) % (xTo - xFrom)
                yOffset  = fromIntegral yFrom - slope * fromIntegral xFrom
            \x -> slope * fromIntegral x + yOffset

        isInt x = fromIntegral (round x) == x

isVisible ::  Set.Set Asteroid -> Asteroid -> Asteroid -> Bool
isVisible allAsteroids from to = do
    let blockingCoordinates = calculateBlockinCoordinates from to
    let blockingAsteroids = Set.intersection allAsteroids blockingCoordinates
    null blockingAsteroids

getVisibleAsteroids :: Set.Set Asteroid -> Asteroid -> Set.Set Asteroid
getVisibleAsteroids allAsteroids from = do
    let allAsteroidsExcludingFrom = Set.delete from allAsteroids
    Set.filter (isVisible allAsteroids from) allAsteroidsExcludingFrom

calculateVisibleAsteroids :: Set.Set Asteroid -> Asteroid -> Int
calculateVisibleAsteroids allAsteroids from = Set.size $ getVisibleAsteroids allAsteroids from

calculateAllVisibleAsteroids :: Set.Set Asteroid -> Set.Set (Asteroid, Int)
calculateAllVisibleAsteroids allAsteroids = do
    Set.map (\asteroid -> (asteroid, calculateVisibleAsteroids allAsteroids asteroid)) allAsteroids

getBestAsteroid = maximumBy (compare `on` snd) . calculateAllVisibleAsteroids

-- Part A:
-- 326
-- (1.026857s)
partA = getBestAsteroid

------------ PART B ------------

-- >>> calculateAngle (XY(0,0)) (XY(-1,0))
-- 4.71238898038469
-- >>> calculateAngle (XY(0,0)) (XY(0,0))
-- 0.0
-- >>> calculateAngle (XY(0,0)) (XY(1,0))
-- 1.5707963267948966
-- >>> calculateAngle (XY(0,0)) (XY(1,1))
-- 2.356194490192345
-- >>> calculateAngle (XY(8,3)) (XY(7,0))
-- 5.961434752782944
-- >>> calculateAngle (XY(8,3)) (XY(8,1))
-- 0.0
-- >>> calculateAngle (XY(8,3)) (XY(9,0))
-- 0.3217505543966422
-- >>> calculateAngle (XY(8,3)) (XY(15,2))
-- 1.4288992721907328
-- >>> calculateAngle (XY(8,3)) (XY(4,4))
-- 4.4674103172578254
-- >>> calculateAngle (XY(8,3)) (XY(12,2))
-- 1.3258176636680326
-- >>> calculateAngle (XY(8,3)) (XY(12,3))
-- 1.5707963267948966
-- >>> calculateAngle (XY(8,3)) (XY(2,3))
-- 4.71238898038469
calculateAngle origin to = do
    let xOrigin = getX origin
        xTo = getX to
        yOrigin = getY origin
        yTo = getY to
        oppositeSide = abs (xTo - xOrigin)
        adjacentSide = abs (yTo - yOrigin)
    if adjacentSide == 0
        then 
            if xTo == xOrigin
                then 0
                else
                    if xTo < xOrigin
                        then 3 * pi / 2
                        else pi / 2 
        else do
            let angle = atan (fromRational (fromIntegral oppositeSide % fromIntegral adjacentSide))
            if yTo < yOrigin
                then 
                    if xTo >= xOrigin
                        then angle
                        else 2 * pi - angle
                else
                    if xTo >= xOrigin
                        then pi - angle
                        else pi + angle
                

-- >>> nextAsteroidToBeVaporized (XY(0,0)) (Set.fromList [XY(0,1), XY(0,2), XY(2,0), XY(2,1)])
-- (fromList [XY (0,2),XY (2,0),XY (2,1)],XY (0,1))
-- >>> nextAsteroidToBeVaporized (XY(0,0)) (Set.fromList [XY(0,2), XY(2,0), XY(2,1)])
-- (fromList [XY (2,0),XY (2,1)],XY (0,2))
-- >>> nextAsteroidToBeVaporized (XY(0,0)) (Set.fromList [XY(2,0), XY(2,1)])
-- (fromList [XY (2,1)],XY (2,0))
-- >>> nextAsteroidToBeVaporized (XY(0,0)) (Set.fromList [XY(2,1)])
-- (fromList [],XY (2,1))
nextAsteroidToBeVaporized :: Coordinate -> Set.Set Asteroid -> (Set.Set Asteroid, Asteroid)
nextAsteroidToBeVaporized laserPosition visibleAsteroids = do
    let nextAsteroid = minimumBy (compare `on` calculateAngle laserPosition) visibleAsteroids
    (Set.delete nextAsteroid visibleAsteroids, nextAsteroid)


findNthAsteroidVaporized nThValue laserPosition allAsteroids = do
    nextRotation 0 allAsteroids
    where
        nextRotation currentCount remainingAsteroids = do
            let visibleAsteroids = getVisibleAsteroids remainingAsteroids laserPosition
            if Set.size visibleAsteroids + currentCount < nThValue
                then
                    nextRotation (Set.size visibleAsteroids + currentCount) (remainingAsteroids Set.\\ visibleAsteroids)
                else do
                    let remainingAsteroid = nThValue - currentCount
                    snd . last . take remainingAsteroid . drop 1 . iterate (nextAsteroidToBeVaporized laserPosition . fst) $ (visibleAsteroids, laserPosition)

-- Part B:
-- 1623
-- (1.044089s)
partB allAsteroids = do
    let stationPosition = fst $ getBestAsteroid allAsteroids
    let n200thAsteroidVaporized = findNthAsteroidVaporized 200 stationPosition (Set.delete stationPosition allAsteroids)
    getX n200thAsteroidVaporized * 100 + getY n200thAsteroidVaporized