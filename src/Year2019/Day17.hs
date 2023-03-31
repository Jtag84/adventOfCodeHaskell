module Year2019.Day17 (runDay) where

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
import Year2019.IntCodeComputer (opcodesParser, runIntCodeProgram, runIntCodeProgramWithMemoryUpdate, getOutputs, Opcode)
import Util.Util (prettyPrintMatrixWith)
import Data.Matrix (Matrix(nrows, ncols))

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------

type Row = Int
type Col = Int

------------ PART A ------------

-- Part A:
-- (
-- ┌                                                                                                       ┐
-- │ . . . . . . . . . . . . . . # # # # # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . # . . . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . # # # # # # # # # # # # # . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . # . . . . . . . . . # . # . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ # # # # # # # # # # # # # . # # # # # # # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ # . . . # . . . . . . . # . . . # . # . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ # . . . # . . . . . . . # . . . # . # . # . . . . . . . . . . . . . . . . . . . . . # # # # # # # # # │
-- │ # . . . # . . . . . . . # . . . # . # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ # # # # # . . . . . . . # . . . # . # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . . . . . . . . . # . . . # . # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . # # # # # # # # # # # # # . # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . # . . . . . . . # . . . . . # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . # . # # # # # # # # # # # # # . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . # . # . . . . . # . . . . . . . # . . . . . . . . . . . . . . . . . . . . . # . . . . . . . # │
-- │ . . . . # . # . . . . . # . . . . . # # # # # # # # # # # # # . . . . . . . # # # # # # # # # # # # # │
-- │ . . . . # . # . . . . . # . . . . . # . # . . . . . . . . . # . . . . . . . # . . . # . . . . . . . . │
-- │ . . . . # . # . . . . . # # # # # # # # # . . . . . . . . . # . . . . . . . # . . . # . . . . . . . . │
-- │ . . . . # . # . . . . . . . . . . . # . . . . . . . . . . . # . . . . . . . # . . . # . . . . . . . . │
-- │ . . . . # . # . # # # # # # # # # . # . . . . . . . . . . . # . . . . . . . # . . . # # # # # # ^ . . │
-- │ . . . . # . # . # . . . . . . . # . # . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . # . # # # # # # # # # # # # # . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . # . . . # . . . . . . . # . . . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . # # # # # . . . . . . . # . . . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . # . . . . . . . # . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . # # # # # # # # # . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . │
-- │ . . . . . . . . . . . . . . . . # # # # # # # # # # # # # . . . . . . . . . . . . . . . . . . . . . . │
-- └                                                                                                       ┘,2080)
-- (0.151660s)
partA opcodes = do
    let cameraView = lines . map toEnum . getOutputs . runIntCodeProgram [] $ opcodes
    let cameraViewMatrix = M.fromLists . init $ cameraView
    (prettyPrintMatrixWith (:[]) cameraViewMatrix, calculateAlignmentParameters . getAllIntersectionCoodinates $ cameraViewMatrix)
    where
        calculateAlignmentParameters = sum . map (\(row, col) -> (row -1) * (col -1))

getAllIntersectionCoodinates :: M.Matrix Char -> [(Row, Col)]
getAllIntersectionCoodinates matrix = [(row, col) | row <- [1..maxRow], col <- [1.. maxCol], isIntersection (row, col)]
    where
        maxRow = nrows matrix
        maxCol = ncols matrix
        isIntersection (row, col) =  
            matrix M.! (row, col) == '#'
                &&(row > 1 && row < maxRow && col > 1 && col < maxCol) 
                && all ((== '#') . (M.!) matrix . toMatrixCoordinate ) (getAroundCoordinatesExcludingDiagonals (Matrix (row,col)))
                

------------ PART B ------------
-- A: L,6,R,12,R,8
-- B: R,8,R,12,L,12
-- C: R,12,L,12,L,4,L,4
-- AAAAAAAAAAAA BBBBBBBBBBBBB BBBBBBBBBBBBB AAAAAAAAAAAA CCCCCCCCCCCCCCCCC AAAAAAAAAAAA CCCCCCCCCCCCCCCCC AAAAAAAAAAAA CCCCCCCCCCCCCCCCC BBBBBBBBBBBBB
-- L,6,R,12,R,8,R,8,R,12,L,12,R,8,R,12,L,12,L,6,R,12,R,8,R,12,L,12,L,4,L,4,L,6,R,12,R,8,R,12,L,12,L,4,L,4,L,6,R,12,R,8,R,12,L,12,L,4,L,4,R,8,R,12,L,12
-- A,B,B,A,C,A,C,A,C,B

-- Part B:
-- 742673
-- (0.343012s)
partB opcodes = do
    let inputs = "A,B,B,A,C,A,C,A,C,B\nL,6,R,12,R,8\nR,8,R,12,L,12\nR,12,L,12,L,4,L,4\nn\n"
    let inputsAscii = map fromEnum inputs
    last . getOutputs . runIntCodeProgramWithMemoryUpdate (0,2) inputsAscii $ opcodes