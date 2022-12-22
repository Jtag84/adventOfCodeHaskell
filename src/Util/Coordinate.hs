module Util.Coordinate where

type LineCoordinate = (Coordinate,Coordinate)

data Coordinate where
  XY :: (Int, Int) -> Coordinate
  Matrix :: (Int, Int) -> Coordinate
  deriving(Show, Eq, Ord)

toMatrixCoordinate :: Coordinate -> (Int, Int)
toMatrixCoordinate (XY (x, y)) = (y + 1, x + 1)
toMatrixCoordinate (Matrix rowColumn) = rowColumn

getX :: Coordinate -> Int
getX (XY (x, _))= x
getX (Matrix (_, column)) = column - 1

getY :: Coordinate -> Int
getY (XY (_, y)) = y
getY (Matrix (row, _)) = row - 1

getRowCoordinate :: Coordinate -> Int
getRowCoordinate = (+) 1 . getY

getColumnCoordinate :: Coordinate -> Int
getColumnCoordinate = (+) 1 . getX

getMaxX :: [LineCoordinate] -> Int
getMaxX = maximum . concatMap (\(left, right) -> [getX left, getX right])

getMaxY :: [LineCoordinate] -> Int
getMaxY = maximum . concatMap (\(left, right) -> [getY left, getY right])
