module Year2016.Day08 (runDay) where

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

------------ TYPES ------------
data Pixel = On | Off
    deriving (Eq)

instance Show Pixel where
    show :: Pixel -> String
    show On  = "#"
    show Off = "."

type Screen = M.Matrix Pixel
type Width = Int
type Height = Int
type Row = Int
type Col = Int
type By = Int

data ScreenTransformation = 
    Rect Width Height 
    | RotateRow Row By
    | RotateCol Col By
    deriving(Eq, Show)
------------ PARSER ------------

inputParser :: Parser [ScreenTransformation]
inputParser = many screenTransformationParser

screenTransformationParser :: Parser ScreenTransformation
screenTransformationParser = P.choice [rectParser, rotateRowParser, rotateColParser] <* (endOfInput <|> endOfLine)

rectParser :: Parser ScreenTransformation
rectParser = do
    "rect "
    Just width <- toBoundedInteger <$> scientific
    "x"
    Just height <- toBoundedInteger <$> scientific
    return $ Rect width height

rotateRowParser :: Parser ScreenTransformation
rotateRowParser = do
    "rotate row y="
    Just row <- toBoundedInteger <$> scientific
    " by "
    Just numberOfPixelsToShift <- toBoundedInteger <$> scientific
    return $ RotateRow (row + 1) numberOfPixelsToShift

rotateColParser :: Parser ScreenTransformation
rotateColParser = do
    "rotate column x="
    Just col <- toBoundedInteger <$> scientific
    " by "
    Just numberOfPixelsToShift <- toBoundedInteger <$> scientific
    return $ RotateCol (col + 1) numberOfPixelsToShift

------------ PART A ------------

screenHeight = 6
screenWidth = 50

startScreen :: Screen
startScreen = M.matrix screenHeight screenWidth (const Off)

rect width height screen = do
    let pixelsToTurnOn = Set.fromList [(row, col) | row <- [1..height], col <- [1..width]]
    M.mapPos (\pixelPosition pixelValue -> if pixelPosition `Set.member` pixelsToTurnOn then On else pixelValue) screen 

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow row by screen = do
    let toShift = by `rem` screenWidth
    let shiftedRow = take screenWidth . drop (screenWidth - toShift) . cycle . Vec.toList . M.getRow row $ screen
    M.mapRow (\col _ -> shiftedRow !! (col - 1)) row screen

rotateCol :: Int -> Int -> Screen -> Screen
rotateCol col by screen = do
    let toShift = by `rem` screenHeight
    let shiftedCol = take screenHeight . drop (screenHeight - toShift) . cycle . Vec.toList . M.getCol col $ screen
    M.mapCol (\row _ -> shiftedCol !! (row - 1)) col screen

applyScreenTransformation :: ScreenTransformation -> Screen -> Screen
applyScreenTransformation (Rect width height) = rect width height
applyScreenTransformation (RotateRow row by) = rotateRow row by
applyScreenTransformation (RotateCol col by) = rotateCol col by

applyAllTransformations = foldl' (flip applyScreenTransformation) startScreen

-- Part A:
-- 106
-- (0.006869s)
partA transformations = do
    let finalScreen = applyAllTransformations transformations
    length . filter (== On) . M.toList $ finalScreen

------------ PART B ------------

-- Part B:
-- ┌                                                                                                     ┐
-- │ . # # . . # # # # . # . . . . # # # # . # . . . . . # # . . # . . . # # # # # . . # # . . . # # # . │
-- │ # . . # . # . . . . # . . . . # . . . . # . . . . # . . # . # . . . # # . . . . # . . # . # . . . . │
-- │ # . . . . # # # . . # . . . . # # # . . # . . . . # . . # . . # . # . # # # . . # . . . . # . . . . │
-- │ # . . . . # . . . . # . . . . # . . . . # . . . . # . . # . . . # . . # . . . . # . . . . . # # . . │
-- │ # . . # . # . . . . # . . . . # . . . . # . . . . # . . # . . . # . . # . . . . # . . # . . . . # . │
-- │ . # # . . # . . . . # # # # . # # # # . # # # # . . # # . . . . # . . # . . . . . # # . . # # # . . │
-- └                                                                                                     ┘
-- (0.007806s)
partB = applyAllTransformations