module Year2018.Day09 (runDay) where

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
import Util.LinkedList as LL
import Util.Range
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

type Marble = Int
type Score = Int
type Player = Int

------------ PARSER ------------

inputParser :: Parser Text
inputParser = takeText

------------ PART A ------------

playMarble :: Marble -> MutableLinkedList s Marble -> ST s (MutableLinkedList s Marble, Score)
playMarble marble currentMarbleInCircle = do
        if marble `rem` 23 == 0 
            then
                do
                    marbleToRemove <- previousNthNode 7 currentMarbleInCircle
                    newCurrentMarble <- nextNode marbleToRemove
                    removeNode marbleToRemove
                    return (newCurrentMarble, marble + (fromJust . getValue $ marbleToRemove))
            else
                do
                    marbleToInsertAfter <- nextNode currentMarbleInCircle
                    marbleToInsert <- toMutableNode marble
                    insertNodeAfter marbleToInsert marbleToInsertAfter
                    return (marbleToInsert, 0)    

playMarbleForPlayer :: Player -> Marble -> MutableLinkedList s Marble -> Map.Map Player Score -> ST s (MutableLinkedList s Marble, Map.Map Player Score)
playMarbleForPlayer player marble currentMarbleInCircle currentScoreByPlayer = do
    (newMarbleInCircle, score) <- playMarble marble currentMarbleInCircle
    return (newMarbleInCircle, Map.alter (maybe (Just 0) (Just . (+) score)) player currentScoreByPlayer)

-- >>> runGame 9 25
-- ([0,16,8,17,4,18,19,2,24,20,25,10,21,5,22,11,1,12,6,13,3,14,7,15],fromList [(1,0),(2,0),(3,0),(4,0),(5,32),(6,0),(7,0),(8,0),(9,0)])
-- >>> maximum . map snd . Map.toList . snd $ runGame 10 1618
-- 8317
-- >>> maximum . map snd . Map.toList . snd $ runGame 13 7999
-- 146373
-- >>> maximum . map snd . Map.toList . snd $ runGame 17 1104
-- 2764
-- >>> maximum . map snd . Map.toList . snd $ runGame 21 6111
-- 54718
-- >>> maximum . map snd . Map.toList . snd $ runGame 30 5807
-- 37305
runGame :: Player -> Marble -> ([Marble], Map.Map Player Score)
runGame numberOfPlayers lastMarbleValue = runST $ do
    let (firstMarble:restOfMarbles) = [0..lastMarbleValue]
    let playersWithMarbles = zip (cycle [1..numberOfPlayers]) restOfMarbles
    startingMarble <- toMutableNode firstMarble
    linkToNext startingMarble startingMarble
    linkToPrevious startingMarble startingMarble
    (_, finalScoreMap) <- foldlM (\(currentMarble, scoreMap) (player, marble) -> playMarbleForPlayer player marble currentMarble scoreMap ) (startingMarble, Map.empty) playersWithMarbles 
    lastMarble <- previousNode startingMarble
    linkToPrevious startingMarble Empty
    linkToNext lastMarble Empty
    finalMarbleCircleList <- LL.toList startingMarble
    return (finalMarbleCircleList, finalScoreMap)

-- Part A:
-- 396136
-- (0.054641s)
partA _ = maximum . map snd . Map.toList . snd $ runGame 463 71787

------------ PART B ------------

-- Part B:
-- 3183301184
-- (7.020761s)
partB _ = maximum . map snd . Map.toList . snd $ runGame 463 (71787 * 100)
