module Year2020.Day05 (runDay) where

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
import Options.Applicative (Alternative (empty), value, (<|>), ParseError)
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import System.Console.ANSI (ConsoleLayer(Background))
import Util.Util (bitsToInt)
import Data.Tuple.All (Sel3(sel3))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [[Int]]
inputParser = many' partitioningSequenceParser

partitioningSequenceParser :: Parser [Int]
partitioningSequenceParser = many1' partitioningParser <* (endOfLine <|> endOfInput)

partitioningParser :: Parser Int
partitioningParser = P.choice [
        "F" $> 0,
        "B" $> 1,
        "L" $> 0,
        "R" $> 1
    ]

------------ PART A ------------
getSeatRowColId [row7,row6,row5,row4,row3,row2,row1,col3,col2,col1] = do
    let rowNumber = bitsToInt [row7,row6,row5,row4,row3,row2,row1]
    let colNumber = bitsToInt [col3,col2,col1]
    (rowNumber, colNumber, rowNumber * 8 + colNumber)

-- Part A:
-- (122,0,976)
-- (0.000150s)
partA = maximumBy (compare `on` sel3) . map getSeatRowColId

------------ PART B ------------

-- Part B:
-- (85,5,685)
-- (0.001058s)
partB boardingPasses = do
    let allPossibleSeats = Set.fromList [(row, col, row * 8 + col) | row <- [1..126], col <- [0..7]]
    let allSeatsWithBoardingPass = Set.fromList $ map getSeatRowColId boardingPasses
    head . Set.toList $ Set.difference allPossibleSeats allSeatsWithBoardingPass

