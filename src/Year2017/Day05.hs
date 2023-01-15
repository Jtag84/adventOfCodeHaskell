module Year2017.Day05 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST, ST)
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
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Tuple.All (Sel3(sel3))
import Data.Array.MArray (readArray, writeArray, MArray (getBounds, newArray), newListArray)
import Data.Array.ST (STArray)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Step = Int
type Index = Int
------------ PARSER ------------

inputParser :: Parser [Int]
inputParser = many1' $ fromJust . toBoundedInteger <$> scientific <* (endOfLine <|> endOfInput)

------------ PART A ------------

-- Part A:
-- 372671
-- (0.030182s)
partA jumps = runST $ do
        jumpsArray <- newListArray (0,length jumps - 1) jumps
        executeJumpSTUntilExits calculateValuePartA (jumpsArray, 0, 0)

calculateValuePartA = (+ 1)
------------ PART B ------------

calculateValuePartB value =
    if value >= 3 
    then 
        value - 1 
    else 
        value + 1

executeJumpSTArray :: (Int->Int) -> (STArray s Int Int, Index, Step) -> ST s (Either (STArray s Int Int, Index, Step) (STArray s Int Int, Index, Step) )
executeJumpSTArray calculateValue (jumps, index, step) = do
        (minIndex, maxIndex) <- getBounds jumps
        if index > maxIndex || index < minIndex
            then 
                return $ Left (jumps, index, step)
            else
                do
                    value <- readArray jumps index
                    let newValue = calculateValue value
                    writeArray jumps index newValue
                    return $ Right (jumps, index + value, step + 1)

executeJumpSTUntilExits :: (Int->Int) -> (STArray s Int Int, Index, Step) -> ST s Step
executeJumpSTUntilExits calculateValue (jumpsArray, index, step) = do
        resultJumpExecution <- executeJumpSTArray calculateValue (jumpsArray, index, step)
        case resultJumpExecution of
            Right arrayIndexStep -> executeJumpSTUntilExits calculateValue arrayIndexStep
            Left (_,_,step) -> return step

-- Part B:
-- 25608480
-- (2.869166s)
partB jumps = runST $ do
        jumpsArray <- newListArray (0,length jumps - 1) jumps
        executeJumpSTUntilExits calculateValuePartB (jumpsArray, 0, 0)