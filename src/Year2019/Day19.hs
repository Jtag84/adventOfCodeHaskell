module Year2019.Day19 (runDay) where

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
import Year2019.IntCodeComputer (opcodesParser, runIntCodeProgram, getOutputs)
import Util.Util (mapToPrettyPrintMatrix)

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------

data DroneState = Stationary | Pulled
    deriving(Eq, Enum)

instance Show DroneState where
    show Stationary = " "
    show Pulled = "■"

------------ PART A ------------

-- Part A:
-- (144,
-- ┌                                                                                                     ┐
-- │ ■                                                                                                   │
-- │                                                                                                     │
-- │                                                                                                     │
-- │                                                                                                     │
-- │                                                                                                     │
-- │                                                                                                     │
-- │           ■                                                                                         │
-- │             ■                                                                                       │
-- │               ■                                                                                     │
-- │                 ■                                                                                   │
-- │                   ■                                                                                 │
-- │                     ■                                                                               │
-- │                     ■ ■                                                                             │
-- │                       ■ ■                                                                           │
-- │                         ■ ■                                                                         │
-- │                           ■ ■                                                                       │
-- │                             ■ ■                                                                     │
-- │                               ■ ■                                                                   │
-- │                               ■ ■ ■                                                                 │
-- │                                 ■ ■                                                                 │
-- │                                   ■ ■                                                               │
-- │                                     ■ ■                                                             │
-- │                                       ■ ■                                                           │
-- │                                         ■ ■                                                         │
-- │                                         ■ ■ ■                                                       │
-- │                                           ■ ■ ■                                                     │
-- │                                             ■ ■ ■                                                   │
-- │                                               ■ ■ ■                                                 │
-- │                                                 ■ ■ ■                                               │
-- │                                                   ■ ■ ■                                             │
-- │                                                   ■ ■ ■ ■                                           │
-- │                                                     ■ ■ ■ ■                                         │
-- │                                                       ■ ■ ■ ■                                       │
-- │                                                         ■ ■ ■ ■                                     │
-- │                                                           ■ ■ ■ ■                                   │
-- │                                                             ■ ■ ■ ■                                 │
-- │                                                             ■ ■ ■ ■ ■                               │
-- │                                                               ■ ■ ■ ■ ■                             │
-- │                                                                 ■ ■ ■ ■                             │
-- │                                                                   ■ ■ ■ ■                           │
-- │                                                                     ■ ■ ■ ■                         │
-- │                                                                     ■ ■ ■ ■ ■                       │
-- │                                                                       ■ ■ ■ ■ ■                     │
-- │                                                                         ■ ■ ■ ■ ■                   │
-- │                                                                           ■ ■ ■ ■ ■                 │
-- │                                                                             ■ ■ ■ ■ ■               │
-- │                                                                               ■ ■ ■ ■ ■             │
-- │                                                                               ■ ■ ■ ■ ■ ■           │
-- │                                                                                 ■ ■ ■ ■ ■ ■         │
-- │                                                                                   ■ ■ ■ ■ ■ ■       │
-- └                                                                                                     ┘)
-- (2.959726s)
partA opcodes = do
    let coordinateToCheck = [(x,y) | x <- [0..49], y <- [0..49]]
    let tractorBeamList = map (\(x,y) -> (XY (x,y), toEnum . head . getOutputs . runIntCodeProgram [x,y] $ opcodes)) coordinateToCheck
    let tractorBeamMap = Map.fromList  tractorBeamList
    let numberOfPulledCoordinate = length . filter ((== Pulled) . snd) $ tractorBeamList
    (numberOfPulledCoordinate,mapToPrettyPrintMatrix show Stationary tractorBeamMap)

------------ PART B ------------
-- By looking at the beam, we can deduct the eqaution to calculate the left and right x coordinates depending on the y. Lx(y) Rx(y)
-- Lx = 34/41 * y
-- Rx = 18/19 * y
-- We want Rx - Lx = 99     (width of 100 total)
-- and we want a height of 100 so By - Ty = 99 (bottom and top y are reverse, y increases going down)
-- we need to find (Lx, Ty)
-- so now we have:
-- Rx = 34/41 * Ty
-- Lx = 18/19 * By
-- we find the following: Lx = 1353, Rx = 1452, Ty = 1533, and By = 1632
-- By using part A to trace the region from the above values we can see that this is not exact, due to rounding, we need to explore the region a bit further by expanding the values to display.
-- From there we find that the correct values are Lx = 1356, Rx = 1455, Ty = 1537, and By = 1636
closestCoordinate = XY(1356, 1537)

-- Part B:
-- 13561537
-- (0.000125s)
partB _ = getX closestCoordinate * 10000 + getY closestCoordinate