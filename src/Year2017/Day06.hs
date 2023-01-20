module Year2017.Day06 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, sepBy1')
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
import Data.Array qualified as A


runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser (A.Array Int Int) 
inputParser = do
    bankList <- (fromJust . toBoundedInteger <$> scientific) `sepBy1'` char '\t'
    return $ A.listArray (0, length bankList - 1) bankList

------------ PART A ------------

-- >>> executeRedistribution $ A.listArray (0, 3) [0,2,7,0]
-- array (0,3) [(0,2),(1,4),(2,1),(3,2)]
executeRedistribution :: A.Array Int Int -> A.Array Int Int
executeRedistribution banks = do
    let (increase, remainingIndexesToAddOneExtra) = maxValue `quotRem` banksSize
    let newValues = zipWith (\index value -> (index, value + getValue index)) allIndexesStartingAfterMaxIndex $ replicate remainingIndexesToAddOneExtra (increase+1) <> replicate (banksSize - remainingIndexesToAddOneExtra) increase
    banks A.// newValues
    where
        getValue index
            | index == maxIndex = 0
            | otherwise = banks A.! index
        (maxIndex, maxValue) = head . last . groupBy (\a b -> snd a == snd b) $ sortBy (compare `on` snd) $ A.assocs banks
        (startIndex, endIndex) = A.bounds banks
        banksSize = endIndex - startIndex + 1
        allIndexesStartingAfterMaxIndex = take banksSize $ drop (maxIndex + 1) $ cycle [startIndex..endIndex]

-- >>> findNumberOfRedisptributionToLoop 0 Set.empty $ A.listArray (0, 3) [0,2,7,0]
-- (5,array (0,3) [(0,2),(1,4),(2,1),(3,2)])
findNumberOfRedisptributionToLoop :: Int -> Set.Set (A.Array Int Int) -> A.Array Int Int -> (Int, A.Array Int Int)
findNumberOfRedisptributionToLoop cycleNumber alreadySeenConfigurationSet banks = do
        let nextRedistribution = executeRedistribution banks
        if nextRedistribution `Set.member` alreadySeenConfigurationSet
            then
                (cycleNumber + 1, nextRedistribution)
            else
                findNumberOfRedisptributionToLoop (cycleNumber + 1) (Set.insert nextRedistribution alreadySeenConfigurationSet) nextRedistribution
                

-- Part A:
-- 14029
-- (0.051639s)
partA = fst . findNumberOfRedisptributionToLoop 0 Set.empty

------------ PART B ------------

-- Part B:
-- 2765
-- (0.058816s)
partB banks = do
    let firstRepetition = snd $ findNumberOfRedisptributionToLoop 0 Set.empty banks
    (+ (-1)) . fst $ findNumberOfRedisptributionToLoop 0 Set.empty firstRepetition
