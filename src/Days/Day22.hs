module Days.Day22 where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Matrix qualified as M
import Util.Coordinate
import qualified Control.Applicative.Combinators as P
import Data.Functor
import Data.Function (on)
import Data.Scientific (toBoundedInteger)
import Options.Applicative ((<|>), Alternative (empty), value)
import Data.Matrix (Matrix(nrows))


runDay :: R.Day
runDay = R.runDay boardInstructionsParser partA partB

------------ PARSER ------------
boardElementParser :: Parser BoardElement
boardElementParser = P.choice [
        char ' ' $> Out,
        char '.' $> Tile,
        char '#' $> Wall
    ]

boardLineParser :: Parser [BoardElement]
boardLineParser = many1' boardElementParser <* endOfLine

boardParser :: Parser Board
boardParser = do
    boardLines <- many1' boardLineParser
    endOfLine
    let maxLineLength = maximum $ map length boardLines
    let adjustedBoardLines = map (\line -> line <> replicate (maxLineLength - length line) Out) boardLines
    return $ M.fromLists adjustedBoardLines

instructionParser :: Parser Instruction
instructionParser =
    P.choice [
        Forward . fromJust . toBoundedInteger <$> scientific,
        char 'R' $> Rotate Clockwise,
        char 'L' $> Rotate CounterClockwise
    ]

instructionsParser :: Parser [Instruction]
instructionsParser = many1' instructionParser <* endOfInput 

boardInstructionsParser :: Parser (Board, [Instruction])
boardInstructionsParser = (,) <$> boardParser <*> instructionsParser

------------ TYPES ------------
data Direction = 
      RightD 
    | DownD
    | LeftD 
    | UpD 
    deriving(Eq, Show, Enum)

data Rotation = Clockwise | CounterClockwise
    deriving(Eq, Show)

rotate :: Rotation -> Direction -> Direction
rotate Clockwise UpD = RightD
rotate Clockwise direction = succ direction
rotate CounterClockwise RightD = UpD
rotate CounterClockwise direction = pred direction

rotatePlayer :: Rotation -> PlayerPosition -> PlayerPosition
rotatePlayer rotation (direction, position) = (rotate rotation direction, position)

data Instruction = Forward Int | Rotate Rotation
    deriving(Show, Eq)

type PlayerPosition = (Direction, Coordinate)

data BoardElement = Tile | Wall | Out
    deriving (Eq)

instance Show BoardElement where
  show Tile = "."
  show Wall = "#"
  show Out  = " "

type Board = M.Matrix BoardElement

type RowBoundaries = ColBoundaries
type ColBoundaries = (Int, Int)

type BoardBoundaries = ([RowBoundaries], [ColBoundaries])
------------ PART A ------------

getBoardBoundaries :: Board -> BoardBoundaries
getBoardBoundaries board = do
    let rowIndices = [1 .. M.nrows board]
    let columnIndices = [1 .. M.ncols board]
    let rowBoudaries = map (getBoardBoundaries . (`M.getRow` board)) rowIndices
    let colBoudaries = map (getBoardBoundaries . (`M.getCol` board)) columnIndices
    (rowBoudaries, colBoudaries)
    where
        getBoardBoundaries :: Vector BoardElement -> (Int, Int) 
        getBoardBoundaries boardElements = do
            let nonOutBoardElements = Vec.findIndices (/= Out) boardElements
            let minIndex = Vec.minimum nonOutBoardElements
            let maxIndex = Vec.maximum nonOutBoardElements
            (minIndex + 1, maxIndex + 1)

moveOneStepForward :: PlayerPosition -> PlayerPosition
moveOneStepForward playerPosition@(RightD, position) = (RightD, modifyCol (+1) position)
moveOneStepForward playerPosition@(LeftD, position) = (LeftD, modifyCol (+ (-1)) position)
moveOneStepForward playerPosition@(DownD, position) = (DownD, modifyRow (+1) position)
moveOneStepForward playerPosition@(UpD, position) = (UpD, modifyRow (+ (-1)) position)

wrapCoordinate :: BoardBoundaries -> PlayerPosition -> PlayerPosition
wrapCoordinate boundaries playerPosition@(RightD, coordinate)
    | getCol coordinate > getMaxRowBoundary boundaries coordinate = (RightD, Matrix (getRow coordinate, getMinRowBoundary boundaries coordinate))
    | otherwise = playerPosition
wrapCoordinate boundaries playerPosition@(LeftD, coordinate)
    | getCol coordinate < getMinRowBoundary boundaries coordinate = (LeftD, Matrix (getRow coordinate, getMaxRowBoundary boundaries coordinate))
    | otherwise = playerPosition
wrapCoordinate boundaries playerPosition@(DownD, coordinate)
    | getRow coordinate > getMaxColBoundary boundaries coordinate = (DownD, Matrix (getMinColBoundary boundaries coordinate, getCol coordinate))
    | otherwise = playerPosition
wrapCoordinate boundaries playerPosition@(UpD, coordinate)
    | getRow coordinate < getMinColBoundary boundaries coordinate = (UpD, Matrix (getMaxColBoundary boundaries coordinate, getCol coordinate))
    | otherwise = playerPosition
    
getMaxRowBoundary :: BoardBoundaries ->  Coordinate -> Int
getMaxRowBoundary boundaries coordinate = snd $ fst boundaries !! (getRow coordinate - 1)

getMinRowBoundary :: BoardBoundaries ->  Coordinate -> Int
getMinRowBoundary boundaries coordinate = fst $ fst boundaries !! (getRow coordinate - 1)
    
getMaxColBoundary :: BoardBoundaries ->  Coordinate -> Int
getMaxColBoundary boundaries coordinate = snd $ snd boundaries !! (getCol coordinate - 1)

getMinColBoundary :: BoardBoundaries ->  Coordinate -> Int
getMinColBoundary boundaries coordinate = fst $ snd boundaries !! (getCol coordinate - 1)

moveForward :: (Board, CoordinateWrapper) -> Int -> PlayerPosition -> PlayerPosition
moveForward _ 0 playerPosition = playerPosition
moveForward boardWithWrapper@(board, coordinateWrapper) numberToMove playerPosition@(direction, position) = do
    let nextPosition@(_, nextCoordinate) = coordinateWrapper $ moveOneStepForward playerPosition
    case board M.! toMatrixCoordinate nextCoordinate of
        Tile -> moveForward boardWithWrapper (numberToMove - 1) nextPosition
        Wall -> playerPosition
        Out -> error "shouldn't happened"

executeInstruction :: (Board, CoordinateWrapper) -> Instruction -> PlayerPosition -> PlayerPosition
executeInstruction _ (Rotate rotation) = rotatePlayer rotation
executeInstruction boardWithBoundaries (Forward numberOfSteps) = moveForward boardWithBoundaries numberOfSteps

type CoordinateWrapper = PlayerPosition -> PlayerPosition

getLastPosition :: (Board, [Instruction]) -> CoordinateWrapper -> PlayerPosition
getLastPosition (board, instructions) coordinateWrapper = do
    let startPosition = (RightD, Matrix (1, fromJust (Vec.findIndex (/= Out) (M.getRow 1 board)) + 1))
    foldl' (flip (executeInstruction (board, coordinateWrapper))) startPosition instructions

calculatePassword :: PlayerPosition -> Int
calculatePassword (direction, position) = 1000 * getRow position + 4 * getCol position + fromEnum direction

-- Part A:
-- 80392
-- (0.005967s)
partA :: (Board, [Instruction]) -> Int
partA boardWithInstructions@(board, _ )= do
    let boundaries = getBoardBoundaries board
    calculatePassword $ getLastPosition boardWithInstructions $ wrapCoordinate boundaries

------------ PART B ------------

--         1111
--         1111
--         1111
--         1111
-- 222233334444
-- 222233334444
-- 222233334444
-- 222233334444
--         55556666
--         55556666
--         55556666
--         55556666

-- >>> (Matrix (0, 9)) `isAbove` topSide1 4
-- True
-- >>> (Matrix (1, 9)) `isAbove` topSide1 4
-- False
isAbove :: Coordinate -> Vector Coordinate -> Bool
isAbove coordinate = Vec.elem (getBelowCoordinate coordinate)

-- >>> (Matrix (9, 7)) `isBelow` bottomSide3 4
-- True
-- >>> (Matrix (8, 7)) `isBelow` bottomSide3 4
-- False
isBelow :: Coordinate -> Vector Coordinate -> Bool
isBelow coordinate = Vec.elem (getAboveCoordinate coordinate)

-- >>> (Matrix (6, 13)) `isOnRightOf` rightSide4 4
-- True
-- >>> (Matrix (6, 12)) `isOnRightOf` rightSide4 4
-- False
isOnRightOf :: Coordinate -> Vector Coordinate -> Bool
isOnRightOf coordinate = Vec.elem (getLeftCoordinate coordinate)

-- >>> (Matrix (9, 8)) `isOnLeftOf` leftSide5 4
-- True
-- >>> (Matrix (9, 9)) `isOnLeftOf` leftSide5 4
-- False
isOnLeftOf :: Coordinate -> Vector Coordinate -> Bool
isOnLeftOf coordinate = Vec.elem (getRightCoordinate coordinate)

getBelowCoordinate :: Coordinate -> Coordinate
getBelowCoordinate coordinate = Matrix (getRow coordinate + 1, getCol coordinate)

getAboveCoordinate :: Coordinate -> Coordinate
getAboveCoordinate coordinate = Matrix (getRow coordinate - 1, getCol coordinate)

getLeftCoordinate :: Coordinate -> Coordinate
getLeftCoordinate coordinate = Matrix (getRow coordinate, getCol coordinate - 1)

getRightCoordinate :: Coordinate -> Coordinate
getRightCoordinate coordinate = Matrix (getRow coordinate, getCol coordinate + 1)

topSide :: (Int,Int) -> Int -> Vector Coordinate
topSide (cubeRow,cubeCol) cubeSize = Vec.fromList $ zipWith (curry Matrix) [topRow, topRow ..] [cubeCol * cubeSize + 1 .. (cubeCol + 1)* cubeSize]
    where
        topRow = cubeRow * cubeSize + 1

bottomSide :: (Int,Int) -> Int -> Vector Coordinate
bottomSide (cubeRow,cubeCol) cubeSize = Vec.fromList $ zipWith (curry Matrix) [bottomRow, bottomRow ..] [cubeCol * cubeSize + 1 .. (cubeCol + 1)* cubeSize]
    where
        bottomRow = (cubeRow + 1) * cubeSize

leftSide :: (Int,Int) -> Int -> Vector Coordinate
leftSide (cubeRow,cubeCol) cubeSize = Vec.fromList $ zipWith (curry Matrix) [cubeRow * cubeSize + 1 .. (cubeRow + 1) * cubeSize] [leftCol, leftCol ..]
    where
        leftCol = cubeCol * cubeSize + 1

rightSide :: (Int,Int) -> Int -> Vector Coordinate
rightSide (cubeRow,cubeCol) cubeSize = Vec.fromList $ zipWith (curry Matrix) [cubeRow * cubeSize + 1 .. (cubeRow + 1) * cubeSize] [rightCol, rightCol ..]
    where
        rightCol = (cubeCol + 1) * cubeSize 

side1Example = (0,2)
side2Example = (1,0)
side3Example = (1,1)
side4Example = (1,2)
side5Example = (2,2)
side6Example = (2,3)

rightSide1Example = rightSide side1Example 4
rightSide4Example = rightSide side4Example 4
rightSide6Example = rightSide side6Example 4
bottomSide2Example = bottomSide side2Example 4
bottomSide3Example = bottomSide side3Example 4
bottomSide5Example = bottomSide side5Example 4
bottomSide6Example = bottomSide side6Example 4
leftSide1Example = leftSide side1Example 4
leftSide2Example = leftSide side2Example 4
leftSide5Example = leftSide side5Example 4
topSide1Example = topSide side1Example 4
topSide2Example = topSide side2Example 4
topSide3Example = topSide side3Example 4
topSide6Example = topSide side6Example 4

-- >>> transposeToSide (Vec.fromList [Matrix (5,12),Matrix (6,12),Matrix (7,12),Matrix (8,12)]) (Vec.reverse (Vec.fromList [Matrix (9,13),Matrix (9,14),Matrix (9,15),Matrix (9,16)])) (Matrix (8, 12))
-- Matrix (9,13)
transposeToSide :: Vector Coordinate -> Vector Coordinate -> Coordinate -> Coordinate
transposeToSide fromSide toSide coordinate = do
    let fromIndex = fromJust $ Vec.findIndex (== coordinate) fromSide
    toSide Vec.! fromIndex

wrapCoordinateOnCubeExample :: CoordinateWrapper
wrapCoordinateOnCubeExample playerPosition@(RightD, position)
    | position `isOnRightOf` rightSide1Example = (LeftD, transposeToSide rightSide1Example (Vec.reverse rightSide6Example) (getLeftCoordinate position))
    | position `isOnRightOf` rightSide4Example = (DownD, transposeToSide rightSide4Example (Vec.reverse rightSide6Example) (getLeftCoordinate position))
    | position `isOnRightOf` rightSide6Example = (LeftD, transposeToSide rightSide6Example (Vec.reverse rightSide1Example) (getLeftCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCubeExample playerPosition@(DownD, position)
    | position `isBelow` bottomSide2Example = (UpD, transposeToSide bottomSide2Example (Vec.reverse bottomSide5Example) (getAboveCoordinate position))
    | position `isBelow` bottomSide3Example = (RightD, transposeToSide bottomSide3Example (Vec.reverse leftSide5Example) (getAboveCoordinate position))
    | position `isBelow` bottomSide5Example = (UpD, transposeToSide bottomSide5Example (Vec.reverse bottomSide2Example) (getAboveCoordinate position))
    | position `isBelow` bottomSide6Example = (UpD, transposeToSide bottomSide5Example (Vec.reverse bottomSide2Example) (getAboveCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCubeExample playerPosition@(LeftD, position)
    | position `isOnLeftOf` leftSide1Example = (DownD, transposeToSide leftSide1Example topSide3Example (getRightCoordinate position))
    | position `isOnLeftOf` leftSide2Example = (UpD, transposeToSide leftSide2Example (Vec.reverse bottomSide6Example) (getRightCoordinate position))
    | position `isOnLeftOf` leftSide5Example = (UpD, transposeToSide leftSide5Example (Vec.reverse bottomSide3Example) (getRightCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCubeExample playerPosition@(UpD, position)
    | position `isAbove` topSide1Example = (DownD, transposeToSide topSide1Example (Vec.reverse topSide2Example) (getBelowCoordinate position))
    | position `isAbove` topSide2Example = (DownD, transposeToSide topSide2Example (Vec.reverse topSide1Example) (getBelowCoordinate position))
    | position `isAbove` topSide3Example = (RightD, transposeToSide topSide3Example leftSide1Example (getBelowCoordinate position))
    | position `isAbove` topSide6Example = (LeftD, transposeToSide topSide6Example (Vec.reverse rightSide4Example) (getBelowCoordinate position))
    | otherwise = playerPosition


side1 = (0,1)
side2 = (0,2)
side3 = (1,1)
side4 = (2,0)
side5 = (2,1)
side6 = (3,0)

leftSide1 = leftSide side1 50
leftSide3 = leftSide side3 50
leftSide4 = leftSide side4 50
leftSide6 = leftSide side6 50
rightSide2 = rightSide side2 50
rightSide3 = rightSide side3 50
rightSide5 = rightSide side5 50
rightSide6 = rightSide side6 50
topSide1 = topSide side1 50
topSide2 = topSide side2 50
topSide4 = topSide side4 50
bottomSide2 = bottomSide side2 50
bottomSide5 = bottomSide side5 50
bottomSide6 = bottomSide side6 50

--   1122
--   1122
--   33
--   33
-- 4455
-- 4455
-- 66
-- 66
wrapCoordinateOnCube :: CoordinateWrapper
wrapCoordinateOnCube playerPosition@(RightD, position)
    | position `isOnRightOf` rightSide2 = (LeftD, transposeToSide rightSide2 (Vec.reverse rightSide5) (getLeftCoordinate position))
    | position `isOnRightOf` rightSide3 = (UpD, transposeToSide rightSide3 bottomSide2 (getLeftCoordinate position))
    | position `isOnRightOf` rightSide5 = (LeftD, transposeToSide rightSide5 (Vec.reverse rightSide2) (getLeftCoordinate position))
    | position `isOnRightOf` rightSide6 = (UpD, transposeToSide rightSide6 bottomSide5 (getLeftCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCube playerPosition@(DownD, position)
    | position `isBelow` bottomSide2 = (LeftD, transposeToSide bottomSide2 rightSide3 (getAboveCoordinate position))
    | position `isBelow` bottomSide5 = (LeftD, transposeToSide bottomSide5 rightSide6 (getAboveCoordinate position))
    | position `isBelow` bottomSide6 = (DownD, transposeToSide bottomSide6 topSide2 (getAboveCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCube playerPosition@(LeftD, position)
    | position `isOnLeftOf` leftSide1 = (RightD, transposeToSide leftSide1 (Vec.reverse leftSide4) (getRightCoordinate position))
    | position `isOnLeftOf` leftSide3 = (DownD, transposeToSide leftSide3 topSide4 (getRightCoordinate position))
    | position `isOnLeftOf` leftSide4 = (RightD, transposeToSide leftSide4 (Vec.reverse leftSide1) (getRightCoordinate position))
    | position `isOnLeftOf` leftSide6 = (DownD, transposeToSide leftSide6 topSide1 (getRightCoordinate position))
    | otherwise = playerPosition
wrapCoordinateOnCube playerPosition@(UpD, position)
    | position `isAbove` topSide1 = (RightD, transposeToSide topSide1 leftSide6 (getBelowCoordinate position))
    | position `isAbove` topSide2 = (UpD, transposeToSide topSide2 bottomSide6 (getBelowCoordinate position))
    | position `isAbove` topSide4 = (RightD, transposeToSide topSide4 leftSide3 (getBelowCoordinate position))
    | otherwise = playerPosition    

-- Part B:
-- 19534
-- (0.014693s)
partB :: (Board, [Instruction]) -> Int
partB boardWithInstructions@(board, _)=
    -- calculatePassword $ getLastPosition boardWithInstructions wrapCoordinateOnCubeExample
    calculatePassword $ getLastPosition boardWithInstructions wrapCoordinateOnCube

