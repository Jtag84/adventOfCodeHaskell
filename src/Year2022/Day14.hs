module Year2022.Day14 where

import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfLine,
    many',
    scientific,
    sepBy',
    string,
  )
import Data.Either (fromLeft, fromRight, isLeft)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Matrix (Matrix (ncols, nrows), extendTo, getElem, getRow, matrix)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Set as S (Set, empty, insert, member, notMember, size, union)
import Program.RunDay qualified as R
import Util.Coordinate
  ( Coordinate (..),
    LineCoordinate,
    getColumnCoordinate,
    getMaxX,
    getMaxY,
    getRowCoordinate,
    getX,
    getY,
  )
import Util.Util qualified as U

-- >>> runDay R.Quiet "input/Day14.txt"
-- (Just 2.061589,Nothing)
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser [LineCoordinate]
inputParser = concat <$> many' lineCoordinatesParser

coordinatesParser :: Parser Coordinate
coordinatesParser = do
  Just x <- toBoundedInteger <$> scientific
  char ','
  Just y <- toBoundedInteger <$> scientific
  return (XY (x, y))

lineCoordinatesParser :: Parser [LineCoordinate]
lineCoordinatesParser = toLineCoordinates <$> coordinatesParser `sepBy'` string " -> " <* endOfLine
  where
    toLineCoordinates (left : right : xs) = (left, right) : toLineCoordinates (right : xs)
    toLineCoordinates [_] = []
    toLineCoordinates [] = []

------------ TYPES ------------

data CaveElement where
  Empty :: CaveElement
  Rock :: CaveElement
  Sand :: CaveElement
  deriving (Eq)

instance Show CaveElement where
  show :: CaveElement -> String
  show Empty = "."
  show Rock = "#"
  show Sand = "o"

type Cave = Matrix CaveElement

getValue :: Coordinate -> Cave -> CaveElement
getValue coordinate = getElem (getRowCoordinate coordinate) (getColumnCoordinate coordinate)

-- >>> createCave [(XY (2,2), XY (2,5))]
-- ┌       ┐
-- │ . . . │
-- │ . . . │
-- │ . . # │
-- │ . . # │
-- │ . . # │
-- │ . . # │
-- └       ┘
createCave :: [LineCoordinate] -> Cave
createCave lineCoordinates = do
  let maxX = getMaxX lineCoordinates + 1
  let maxY = getMaxY lineCoordinates + 1
  matrix maxY maxX (matrixSetter . Matrix)
  where
    matrixSetter matrixCoordinate
      | any (isOnLine matrixCoordinate) lineCoordinates = Rock
      | otherwise = Empty

isOnLine :: Coordinate -> LineCoordinate -> Bool
isOnLine point (start, end)
  | y == startY && y == endY = signum ((endX - x) * (x - startX)) >= 0
  | x == startX && x == endX = signum ((endY - y) * (y - startY)) >= 0
  | otherwise = False
  where
    x = getX point
    y = getY point
    startX = getX start
    startY = getY start
    endX = getX end
    endY = getY end

-- Left will return an early termination if the sand fell off the matrix, and Right will be if no termination happened, if it filled it up all the way to the start
getRestingSandCoordinate :: Coordinate -> Either (Set Coordinate) (Set Coordinate) -> Cave -> Either (Set Coordinate) (Set Coordinate)
getRestingSandCoordinate _ finalResult@(Left _) _ = finalResult
getRestingSandCoordinate fallingSandCoordinate eitherRestingSands@(Right restingSands) cave
  | row > nrows cave || column > ncols cave = Left restingSands
  | currentValue == Empty && fallingSandCoordinate `notMember` restingSands = do
      let downRestingSandSet = getRestingSandCoordinate nextDownCoordinate eitherRestingSands cave
      let downLeftRestingSandSet = getRestingSandCoordinate nextLeftDownCoordinate downRestingSandSet cave
      let downRightRestingSandSet = getRestingSandCoordinate nextRightDownCoordinate downLeftRestingSandSet cave
      S.insert fallingSandCoordinate <$> downRightRestingSandSet
  | otherwise = eitherRestingSands
  where
    row = getRowCoordinate fallingSandCoordinate
    column = getColumnCoordinate fallingSandCoordinate
    currentValue = getValue fallingSandCoordinate cave
    x = getX fallingSandCoordinate
    y = getY fallingSandCoordinate
    nextDownCoordinate = XY (x, y + 1)
    nextLeftDownCoordinate = XY (x - 1, y + 1)
    nextRightDownCoordinate = XY (x + 1, y + 1)

------------ PART A ------------
partA :: [LineCoordinate] -> Int
partA = S.size . fromLeft S.empty . getRestingSandCoordinate (XY (500, 0)) (Right S.empty) . createCave

------------ PART B ------------
partB :: [LineCoordinate] -> Int
partB lines = S.size . fromRight S.empty . getRestingSandCoordinate (XY (500, 0)) (Right S.empty) $ caveWithFloor
  where
    cave = createCave lines
    cave1EmpyRow = extendTo Empty (nrows cave + 1) (ncols cave + 1000) cave
    caveWithFloor = extendTo Rock (nrows cave1EmpyRow + 1) (ncols cave1EmpyRow) cave1EmpyRow
