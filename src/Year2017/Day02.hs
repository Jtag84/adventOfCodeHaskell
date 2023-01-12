module Year2017.Day02 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, sepBy')
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

inputParser :: Parser [[Int]]
inputParser = many1' rowParser

rowParser :: Parser [Int]
rowParser = (fromJust . toBoundedInteger <$> P.scientific) `sepBy'` char '\t' <* endOfLine

------------ PART A ------------
calculateRowChecksum row = maximum row - minimum row

-- Part A:
-- 48357
-- (0.000023s)
partA = sum . map calculateRowChecksum

------------ PART B ------------

findEvenlyDivisiblePair :: Integral a => [a] -> (a, a)
findEvenlyDivisiblePair row = do
    let rowValuesSet = Set.fromList row
    head . Set.toList -- there should be only one pair so only get the first one
        . Set.filter ((== 0) . uncurry rem)  -- check that the rest of the division is 0
        . Set.filter (uncurry (/=)) -- remove same values from the cartesian product
        $ Set.cartesianProduct rowValuesSet rowValuesSet

-- Part B:
-- 351
-- (0.000507s)
partB = sum . map (uncurry div . findEvenlyDivisiblePair)