module Year2018.Day05 (runDay) where

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
import Data.Char (isAsciiLower, toUpper)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser String
inputParser = many' letter

------------ PART A ------------

isOppositePolarity :: Char -> Char -> Bool
isOppositePolarity leftUnit rightUnit = abs (fromEnum leftUnit - fromEnum rightUnit) == 32

removeOppositePolarities :: MutableLinkedList s Char -> MutableLinkedList s Char -> ST s (MutableLinkedList s Char)
removeOppositePolarities head Empty = return head
removeOppositePolarities head node = do
    previous <- previousNode node
    next <- nextNode node
    nextNextNode <- nextNode next
    if fromMaybe False $ isOppositePolarity <$> getValue next <*> getValue node
        then
            do
                removeNode node
                removeNode next
                case previous of
                    Empty -> removeOppositePolarities nextNextNode nextNextNode
                    _ -> removeOppositePolarities head previous
        else
            removeOppositePolarities head next

-- Part A:
-- 11242
-- (0.013811s)
partA = length . reactPolymer

reactPolymer polymer = runST $ do
    polymerLinkedList <- LL.fromList polymer
    newHead <- removeOppositePolarities polymerLinkedList polymerLinkedList
    LL.toList newHead

------------ PART B ------------

getAllUnitTypes :: String -> String
getAllUnitTypes = map head . group . sort . filter isAsciiLower

-- Part B:
-- 5492
-- (0.300078s)
partB polymer = do
    let allunitTypes = getAllUnitTypes polymer
    let allPolymerWithUnitRemoved = map (\unit -> (filter (/= unit) . filter (/= toUpper unit)) polymer) allunitTypes
    minimum $ map (length . reactPolymer) allPolymerWithUnitRemoved
