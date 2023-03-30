module Year2018.Day10 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, space)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger, normalize)
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
import qualified Util.Coordinate as M
import Util.Util (prettyPrintMatrix)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Position = Coordinate
type Velocity = Coordinate

type Lights = [(Position, Velocity)]
type Seconds = Int
------------ PARSER ------------

inputParser :: Parser Lights
inputParser = many' parsePositionVelocity

parseCoordinate :: Parser Coordinate
parseCoordinate = do
    "<"
    many' space
    Just x <- toBoundedInteger <$> scientific
    ","
    many' space
    Just y <- toBoundedInteger <$> scientific
    ">"
    return $ XY (x,y)

parsePosition :: Parser Position
parsePosition = "position=" >> parseCoordinate

parseVelocity :: Parser Velocity
parseVelocity = "velocity=" >> parseCoordinate

parsePositionVelocity :: Parser (Position, Velocity)
parsePositionVelocity = (,) <$> parsePosition <* space <*> parseVelocity <* (endOfLine <|> endOfInput)

------------ PART A ------------
oneSecondPassing :: (Lights, Seconds) -> (Lights, Seconds)
oneSecondPassing (lights, seconds) = (normalize . map (\(position, velocity) -> (position `addCoordinates` velocity, velocity)) $ lights, seconds + 1)
    where
        normalize = normalizeCoordinates fst (\newPosition (position, velocity) -> (newPosition, velocity))

getMessage lights = do
    let message = until (withinDisplay . map fst . fst) oneSecondPassing (lights, 0)
    (toMatrix . map fst . fst $ message, snd message)
    where
        withinDisplay positions = maxRow positions < 20

        toMatrix positions = do
            let matrixCoordinatesPositions = map toMatrixCoordinate positions
            let rowSize = maxRow positions
            let colSize = maxCol positions 
            prettyPrintMatrix $ M.matrix rowSize colSize (\matrixCoordinate -> if matrixCoordinate `elem` matrixCoordinatesPositions then "■" else " ")

-- Part A:
-- (
-- ┌                                                                                                                             ┐
-- │ ■               ■ ■ ■ ■ ■         ■ ■ ■ ■       ■ ■ ■ ■ ■       ■ ■ ■ ■ ■       ■         ■     ■ ■ ■ ■ ■ ■     ■ ■ ■ ■ ■ ■ │
-- │ ■               ■         ■     ■         ■     ■         ■     ■         ■     ■         ■     ■                         ■ │
-- │ ■               ■         ■     ■               ■         ■     ■         ■     ■         ■     ■                         ■ │
-- │ ■               ■         ■     ■               ■         ■     ■         ■     ■         ■     ■                       ■   │
-- │ ■               ■ ■ ■ ■ ■       ■               ■ ■ ■ ■ ■       ■ ■ ■ ■ ■       ■ ■ ■ ■ ■ ■     ■ ■ ■ ■ ■             ■     │
-- │ ■               ■     ■         ■     ■ ■ ■     ■               ■         ■     ■         ■     ■                   ■       │
-- │ ■               ■       ■       ■         ■     ■               ■         ■     ■         ■     ■                 ■         │
-- │ ■               ■       ■       ■         ■     ■               ■         ■     ■         ■     ■               ■           │
-- │ ■               ■         ■     ■       ■ ■     ■               ■         ■     ■         ■     ■               ■           │
-- │ ■ ■ ■ ■ ■ ■     ■         ■       ■ ■ ■   ■     ■               ■ ■ ■ ■ ■       ■         ■     ■ ■ ■ ■ ■ ■     ■ ■ ■ ■ ■ ■ │
-- └                                                                                                                             ┘,10011) 
-- LRGPBHEZ
-- Part B answer: 10011 seconds to display                                                                                                                                                                                                                                                    ┘]
-- (1.127204s)

partA = getMessage

------------ PART B ------------

partB _ = "see part A"