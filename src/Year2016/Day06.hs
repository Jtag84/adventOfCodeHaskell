module Year2016.Day06 (runDay) where

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
import qualified Data.List as L

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [String]
inputParser = many1' (many1' letter <* (endOfInput <|> endOfLine))

------------ PART A ------------

getMostRepeatedCharForEachPosition = map (head . maximumBy (compare `on` length) . group . sort) . L.transpose

-- Part A:
-- "qqqluigu"
-- (0.001412s)
partA = getMostRepeatedCharForEachPosition

------------ PART B ------------

getLeastRepeatedCharForEachPosition = map (head . minimumBy  (compare `on` length) . group . sort) . L.transpose

-- Part B:
-- "lsoypmia"
-- (0.001547s)
partB = getLeastRepeatedCharForEachPosition