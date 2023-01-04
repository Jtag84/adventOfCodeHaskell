module Util.Coordinate where
import Data.Vector qualified as Vec
import Data.Vector(Vector)
 
type LineCoordinate = (Coordinate,Coordinate)

data Coordinate where
  XY :: (Int, Int) -> Coordinate
  Matrix :: (Int, Int) -> Coordinate
  deriving(Show, Eq, Ord)

data Direction = 
      RightD 
    | DownD
    | LeftD 
    | UpD 
    deriving(Eq, Show, Enum, Ord)

data Rotation = Clockwise | CounterClockwise
    deriving(Eq, Show)

rotate :: Rotation -> Direction -> Direction
rotate Clockwise UpD = RightD
rotate Clockwise direction = succ direction
rotate CounterClockwise RightD = UpD
rotate CounterClockwise direction = pred direction

toMatrixCoordinate :: Coordinate -> (Int, Int)
toMatrixCoordinate (XY (x, y)) = (y + 1, x + 1)
toMatrixCoordinate (Matrix rowColumn) = rowColumn

getX :: Coordinate -> Int
getX (XY (x, _))= x
getX (Matrix (_, column)) = column - 1


getY :: Coordinate -> Int
getY (XY (_, y)) = y
getY (Matrix (row, _)) = row - 1

getRow = (+1) . getY
getCol = (+1) . getX

getRowCoordinate :: Coordinate -> Int
getRowCoordinate = (+) 1 . getY

getColumnCoordinate :: Coordinate -> Int
getColumnCoordinate = (+) 1 . getX

getMaxX :: [LineCoordinate] -> Int
getMaxX = maximum . concatMap (\(left, right) -> [getX left, getX right])

getMaxY :: [LineCoordinate] -> Int
getMaxY = maximum . concatMap (\(left, right) -> [getY left, getY right])

-- >>> modifyY (+4) (XY (1,3))
-- XY (1,7)
-- >>> modifyY (+4) (Matrix (1,3))
-- Matrix (1,7)
modifyY :: (Int -> Int) -> Coordinate -> Coordinate
modifyY yModifier coordinate@(XY _) = XY (getX coordinate, (yModifier . getY) coordinate)
modifyY yModifier coordinate@(Matrix _) = Matrix (getRow coordinate, (yModifier . getCol) coordinate)

-- >>> modifyX (+4) (XY (1,3))
-- XY (5,3)
modifyX :: (Int -> Int) -> Coordinate -> Coordinate
modifyX xModifier coordinate@(XY _) = XY ((xModifier . getX) coordinate, getY coordinate)
modifyX xModifier coordinate@(Matrix _) = Matrix ((xModifier . getRow) coordinate, getCol coordinate)

-- >>> modifyRow (+4) (Matrix (1,3))
-- Matrix (5,3)
modifyRow :: (Int -> Int) -> Coordinate -> Coordinate
modifyRow = modifyX

-- >>> modifyCol (+4) (Matrix (1,3))
-- Matrix (1,7)
modifyCol :: (Int -> Int) -> Coordinate -> Coordinate
modifyCol = modifyY

minX :: Vector Coordinate -> Int
minX = minimum . Vec.map getX

maxX :: Vector Coordinate -> Int
maxX = maximum . Vec.map getX

minY :: Vector Coordinate -> Int
minY = minimum . Vec.map getY

maxY :: Vector Coordinate -> Int
maxY = maximum . Vec.map getY

getNorth (XY (x,y)) = XY (x,y - 1)
getNorthEast (XY (x,y)) = XY (x + 1,y - 1)
getNorthWest (XY (x,y)) = XY (x - 1,y - 1)
getSouth (XY (x,y)) = XY (x,y + 1)
getSouthEast (XY (x,y)) = XY (x + 1,y + 1)
getSouthWest (XY (x,y)) = XY (x - 1,y + 1)
getWest (XY (x,y)) = XY (x - 1,y)
getEast (XY (x,y)) = XY (x + 1,y)

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance from to = abs (getX from - getX to) + abs (getY from - getY to)