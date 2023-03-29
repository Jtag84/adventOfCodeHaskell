{-# LANGUAGE OverloadedStrings #-}

module Year2019.Day13 (runDay) where

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
import Year2019.IntCodeComputer
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.Text as T
import Util.Util (prettyPrintMatrix)
import Safe (headMay)

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------
data TileType = EmptyTile | Wall | Block | HorizontalPaddle | Ball
    deriving(Eq, Enum)

type Score = Int

instance Show TileType where
  show EmptyTile = " "
  show Wall = "■"
  show Block = "▢"
  show HorizontalPaddle = "_"
  show Ball = "●"

type Tile = (Coordinate, TileType)

type PaddleDirection = Int
------------ PART A ------------

toScoreAndTiles :: (Maybe Score, [Tile]) -> Outputs -> (Maybe Score, [Tile])
toScoreAndTiles currentScoreAndTiles [] = currentScoreAndTiles
toScoreAndTiles (_, currentTiles) (-1:0:score:rest) = toScoreAndTiles (Just score, currentTiles) rest
toScoreAndTiles (maybeScore, currentTiles) (x:y:tileType:rest) = toScoreAndTiles (maybeScore, (XY(x,y), toEnum tileType):currentTiles) rest

-- Part A:
-- (341,
-- ┌                                                                                         ┐
-- │ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ │
-- │ ■                                                                                     ■ │
-- │ ■   ▢ ▢ ▢ ▢     ▢ ▢   ▢     ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢   ▢   ▢   ▢   ▢   ▢   ▢   ▢   ▢     ▢   ■ │
-- │ ■   ▢     ▢   ▢ ▢ ▢   ▢   ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢   ▢       ▢ ▢     ▢ ▢ ▢ ▢   ■ │
-- │ ■   ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢       ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢     ▢ ▢ ▢ ▢ ▢ ▢   ■ │
-- │ ■   ▢   ▢ ▢ ▢ ▢ ▢     ▢     ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢     ▢   ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢   ■ │
-- │ ■   ▢ ▢ ▢   ▢     ▢ ▢   ▢ ▢ ▢   ▢   ▢ ▢ ▢ ▢ ▢       ▢     ▢ ▢ ▢   ▢ ▢       ▢ ▢ ▢     ■ │
-- │ ■   ▢ ▢ ▢ ▢ ▢ ▢ ▢     ▢ ▢ ▢ ▢     ▢ ▢ ▢   ▢ ▢ ▢   ▢ ▢ ▢ ▢   ▢   ▢ ▢   ▢ ▢ ▢   ▢   ▢   ■ │
-- │ ■   ▢ ▢ ▢ ▢   ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢       ▢ ▢ ▢   ▢   ▢   ▢ ▢ ▢ ▢   ▢ ▢   ▢   ▢   ▢     ■ │
-- │ ■   ▢ ▢ ▢ ▢   ▢ ▢   ▢ ▢       ▢   ▢ ▢ ▢ ▢   ▢       ▢ ▢   ▢ ▢ ▢ ▢     ▢ ▢ ▢ ▢   ▢ ▢   ■ │
-- │ ■   ▢ ▢ ▢   ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢     ▢ ▢ ▢ ▢   ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢   ■ │
-- │ ■   ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢   ▢ ▢     ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢   ▢   ▢ ▢     ▢ ▢   ▢ ▢ ▢   ▢ ▢   ■ │
-- │ ■   ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢   ▢   ▢ ▢ ▢   ▢     ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢   ■ │
-- │ ■     ▢ ▢ ▢ ▢ ▢ ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢   ▢ ▢ ▢ ▢ ▢ ▢ ▢ ▢   ▢     ▢ ▢   ▢   ▢ ▢ ▢ ▢ ▢   ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                       ●                                             ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                           _                                         ■ │
-- │ ■                                                                                     ■ │
-- └                                                                                         ┘)
-- (0.111499s)
partA opcodes = do
    let programState = runIntCodeProgram [] opcodes
    (length . filter (== Block) . map snd . snd . toScoreAndTiles (Nothing, []) . getOutputs $ programState, snd . showScreen $ programState)

------------ PART B ------------
-- (Just 17138,
-- ┌                                                                                         ┐
-- │ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■   ●                                                                                 ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■                                                                                     ■ │
-- │ ■   _                                                                                 ■ │
-- │ ■                                                                                     ■ │
-- └                                                                                         ┘)
-- (25.926749s)
partB opcodes = do
    let programMemory = Map.fromList $ zip [0..] opcodes
    let programMemoryWithUnlimitedCoins = Map.insert 0 2 programMemory
    let programStateStart = movePaddle (stay, Running 0 0 [] programMemoryWithUnlimitedCoins [])
    showScreen $ until isProgramStopped (movePaddle . calculatePaddleDirection) programStateStart

showScreen :: ProgramState -> (Maybe Score, M.Matrix U.UnShow)
showScreen programState = do
    let outputs = getOutputs programState
    let (score, tiles) = toScoreAndTiles (Nothing, []) outputs
    let matrixTiles = map (bimap toMatrixCoordinate show) tiles
    let tilesMap = Map.fromList . reverse $ matrixTiles -- need to reverse the outputs so that the last tile output will be kept (in the ouputs the head is the last)
    let positions = map fst matrixTiles
    let rowSize = maximum $ map fst positions
    let colSize = maximum $ map snd positions
    let screen = M.matrix rowSize colSize (tilesMap Map.!)
    (score, prettyPrintMatrix screen)

stay :: PaddleDirection
stay = 0

movePaddle :: (PaddleDirection, ProgramState) -> ProgramState
movePaddle (paddleDirection, programState) = executeUntilNeedsInputOrStopped $ setInputs [paddleDirection] programState

calculatePaddleDirection :: ProgramState -> (PaddleDirection, ProgramState)
calculatePaddleDirection programState = do
    let tiles = snd . toScoreAndTiles (Nothing, []) . getOutputs $ programState
    let currentBallPosition = fst . head . filterTileType Ball $ tiles
    let paddlePosition = fst . head . filterTileType HorizontalPaddle $ tiles
    (signum $ getX currentBallPosition - getX paddlePosition, programState)
    where
        filterTileType tileType = filter (\(position, tile) -> tile == tileType)
