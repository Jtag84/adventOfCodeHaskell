module Year2019.Day08 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, digit)
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
import Util.Util (prettyPrintMatrix)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
pixelWidth = 25
pixelHeight = 6

type Layer = [Int]
------------ PARSER ------------

inputParser :: Parser [Layer]
inputParser = many' layerParser <* (endOfLine <|> endOfInput)

layerParser :: Parser Layer
layerParser = do
    digits <- count (pixelWidth * pixelHeight) digit
    return $ map (flip (-) (fromEnum '0') . fromEnum) digits
    
------------ PART A ------------

-- Part A:
-- 2048
-- (0.006172s)
partA layers = do
    let minZeroes = map length . group . minimumBy (compare `on` (length . filter (== 0))) . map sort $ layers
    minZeroes !! 1 * minZeroes !! 2

------------ PART B ------------

-- Part B:
-- ┌                                                   ┐
-- │ ■     ■   ■ ■ ■ ■   ■       ■   ■ ■     ■     ■   │
-- │ ■     ■   ■         ■       ■ ■     ■   ■   ■     │
-- │ ■ ■ ■ ■   ■ ■ ■       ■   ■   ■     ■   ■ ■       │
-- │ ■     ■   ■             ■     ■ ■ ■ ■   ■   ■     │
-- │ ■     ■   ■             ■     ■     ■   ■   ■     │
-- │ ■     ■   ■             ■     ■     ■   ■     ■   │
-- └                                                   ┘
-- (0.000485s)
partB = prettyPrintMatrix . M.fromList pixelHeight pixelWidth . map (toPixel . head . dropWhile (== 2)) . transpose
    where
        toPixel 0 = " "
        toPixel 1 = "■"