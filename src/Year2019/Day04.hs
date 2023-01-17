module Year2019.Day04 (runDay) where

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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser Text
inputParser = takeText

------------ PART A ------------
startRange = 197487
endRange = 673251

hasTwoSameAdjacentDigit :: String -> Bool
hasTwoSameAdjacentDigit = any ((>=2) . length ) . group

digitAreSameOrIncrease :: String -> Bool
digitAreSameOrIncrease (x1:x2:xs) = x1 <= x2 && digitAreSameOrIncrease (x2:xs)
digitAreSameOrIncrease _ = True

-- Part A:
-- 1640
-- (0.027523s)
partA _ = length
    .  filter hasTwoSameAdjacentDigit 
    . filter digitAreSameOrIncrease 
    $ map show [startRange..endRange]

------------ PART B ------------
hasExactlyTwoSameAdjacentDigit :: String -> Bool
hasExactlyTwoSameAdjacentDigit = any ((==2) . length ) . group

-- Part B:
-- 1126
-- (0.019915s)
partB _ = length
    . filter hasExactlyTwoSameAdjacentDigit 
    . filter digitAreSameOrIncrease 
    $ map show [startRange..endRange]