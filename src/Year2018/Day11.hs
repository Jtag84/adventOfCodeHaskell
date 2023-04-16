module Year2018.Day11 (runDay) where

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
import Data.Scientific (Scientific, toBoundedInteger, scientific)
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
import Data.Tuple.All (Sel3(sel3))

runDay :: R.Day
runDay = R.runDay inputParser partA partB
------------ PARSER ------------

inputParser :: Parser Int
inputParser = fromJust . toBoundedInteger <$> P.scientific

------------ PART A ------------
gridToTraverse squareSize = [XY (x,y) | x <- [1.. 300 - squareSize +1], y <- [1..300 - squareSize + 1]]

-- >>> evalState (calculatePower 8 (XY(3,5))) Map.empty
-- 4
calculatePower :: Int -> Coordinate -> State (Cache Coordinate Int) Int
calculatePower serialNumber coordinate = do
    caching coordinate caculate
    where
        get100thDigit = flip rem 10 . flip div 100
        caculate coordinate = do
            let rackId = getX coordinate + 10
            let powerLevel = rackId * getY coordinate
            get100thDigit ((powerLevel + serialNumber) * rackId) - 5

-- >>> evalState (calculateTotalPowerOnSquare 1 2 (XY(1,1))) Map.empty
-- (-13,XY (1,1))
calculateTotalPowerOnSquare :: Int -> Int -> Coordinate -> State (Cache Coordinate Int) (Int, Coordinate)
calculateTotalPowerOnSquare serialNumber squareSize coordinate = do
    let allSquareCoordinates = [XY (x,y) | x <- [getX coordinate .. getX coordinate + squareSize - 1], y <- [getY coordinate .. getY coordinate + squareSize - 1]]
    totalPower <- sum <$> mapM (calculatePower serialNumber) allSquareCoordinates
    return (totalPower, coordinate)

maximumPowerForSquareSize :: Int -> Int -> State (Cache Coordinate Int) (Int, Coordinate)
maximumPowerForSquareSize serialNumber squareSize = maximum <$> mapM (calculateTotalPowerOnSquare serialNumber squareSize) (gridToTraverse squareSize)

-- Part A:
-- (29,XY (233,36))
-- (0.329141s)
partA :: Int -> (Int, Coordinate)
partA serialNumber = evalState (maximumPowerForSquareSize serialNumber 3) Map.empty

------------ PART B ------------
-- >>> evalState (calculateNewPowerForBiggerSquare 9445 (3, XY(2,2),3)) Map.empty
-- Just (-7,XY (2,2),4)
-- >>> evalState (calculateNewPowerForBiggerSquare 9445 (3, XY(233,36),1)) Map.empty
-- Just (9,XY (233,36),2)
-- >>> evalState (calculateNewPowerForBiggerSquare 9445 (9,XY (233,36),2)) Map.empty
-- Just (29,XY (233,36),3)
calculateNewPowerForBiggerSquare serialNumber (power, coordinate, squareSize) = do
    let newSquareSize = squareSize + 1
    if (getX coordinate + newSquareSize - 1) > 300 || (getY coordinate + newSquareSize - 1) > 300
        then return Nothing
        else do
            let additionalCoordinates = [XY (x,getY coordinate + newSquareSize -1) | x <- [getX coordinate .. getX coordinate + newSquareSize - 1]] ++ [XY (getX coordinate + newSquareSize - 1, y) | y <- [getY coordinate .. getY coordinate + newSquareSize - 2]]
            additionalPower <- sum <$> mapM (calculatePower serialNumber) additionalCoordinates
            return $ Just (power + additionalPower, coordinate, newSquareSize)

calculatePowerForBiggerSquares :: Int -> [(Int, Coordinate, Int)] -> State (Cache Coordinate Int) [(Int, Coordinate, Int)]
calculatePowerForBiggerSquares serialNumber currentSquares = do
    let maxSquareSize = maximum . map sel3 $ currentSquares
    let maxPower = maximum currentSquares
    let currentBiggestSquares = filter ((==maxSquareSize) . sel3) currentSquares
    newBiggerSquares <- catMaybes <$> mapM (calculateNewPowerForBiggerSquare serialNumber) currentBiggestSquares
    return $ maxPower:newBiggerSquares

-- Part B:
-- after 14 iteration the max is reached 
-- (156,XY (231,107),14)
-- (6.986404s)
partB :: Int -> (Int, Coordinate, Int)
partB serialNumber = flip evalState Map.empty $ do
        let initialSquares = mapM (\coordinate -> (,coordinate,1) <$> calculatePower serialNumber coordinate) $ gridToTraverse 1
        maximum <$> (last . take 14 $ iterate (calculatePowerForBiggerSquares serialNumber =<<) initialSquares)