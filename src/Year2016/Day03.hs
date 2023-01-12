module Year2016.Day03 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, space)
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

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type SideLength = Int
type Triangle = (SideLength, SideLength, SideLength)

------------ PARSER ------------

inputParser :: Parser [Triangle]
inputParser = many1' triangleParser

sideLengthParser :: Parser SideLength
sideLengthParser = many' space *> (fromJust . toBoundedInteger <$> P.scientific)

triangleParser :: Parser Triangle
triangleParser = (,,) <$> sideLengthParser <*> sideLengthParser <*> sideLengthParser <* (endOfInput <|> endOfLine)

------------ PART A ------------
isValidTriangle (sideA,sideB,sideC) = (sideA + sideB > sideC) && (sideA + sideC > sideB) && (sideB+sideC > sideA)

-- Part A:
-- 982
-- (0.000273s)
partA = length . filter isValidTriangle

------------ PART B ------------
toColumnTriangles :: [Triangle] -> [Triangle]
toColumnTriangles [] = []
toColumnTriangles ((a1,b1,c1):(a2,b2,c2):(a3,b3,c3):remainingTriangles) = [(a1,a2,a3), (b1,b2,b3), (c1,c2,c3)] <> toColumnTriangles remainingTriangles

-- Part B:
-- 1826
-- (0.000080s)
partB = length . filter isValidTriangle . toColumnTriangles