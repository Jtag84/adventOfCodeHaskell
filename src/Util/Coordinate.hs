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

data CardinalDirection =
      East
    | South
    | West
    | North
    deriving(Eq, Show, Enum, Ord)

addCoordinates coordinate1 coordinate2 = modifyX (\x -> x + getX coordinate2) . modifyY (\y -> y + getY coordinate2) $ coordinate1
subtractCoordinates coordinate1 coordinate2 = modifyX (\x -> x - getX coordinate2) . modifyY (\y -> y - getY coordinate2) $ coordinate1

toCardinalDirection :: Direction -> CardinalDirection
toCardinalDirection = toEnum . fromEnum 

toDirection :: CardinalDirection -> Direction
toDirection = toEnum . fromEnum 

data Rotation = CounterClockwise | Clockwise
    deriving(Eq, Show, Enum)

rotate :: Rotation -> Direction -> Direction
rotate Clockwise UpD = RightD
rotate Clockwise direction = succ direction
rotate CounterClockwise RightD = UpD
rotate CounterClockwise direction = pred direction

rotateCardinal :: Rotation -> CardinalDirection -> CardinalDirection
rotateCardinal rotation cardinalDirection = toCardinalDirection $ rotate rotation (toDirection cardinalDirection)

moveInCardinalDirection :: Coordinate -> CardinalDirection -> Int -> Coordinate
moveInCardinalDirection coordinate North numberNorth = modifyY (+ (-numberNorth)) coordinate
moveInCardinalDirection coordinate East numberEast = modifyX (+ numberEast) coordinate
moveInCardinalDirection coordinate West numberWest = modifyX (+ (-numberWest)) coordinate
moveInCardinalDirection coordinate South numberSouth = modifyY (+ numberSouth) coordinate

moveInDirection coordinate direction = moveInCardinalDirection coordinate (toCardinalDirection direction)

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

minX :: (Foldable t, Functor t) => t Coordinate -> Int
minX = minimum . fmap getX

maxX :: (Foldable t, Functor t) => t Coordinate -> Int
maxX = maximum . fmap getX

minY :: (Foldable t, Functor t) => t Coordinate -> Int
minY = minimum . fmap getY

maxY ::(Foldable t, Functor t) => t Coordinate -> Int
maxY = maximum . fmap getY

minRow :: (Foldable t, Functor t) => t Coordinate -> Int
minRow = minimum . fmap getRow

maxRow ::(Foldable t, Functor t) => t Coordinate -> Int
maxRow = maximum . fmap getRow

minCol :: (Foldable t, Functor t) => t Coordinate -> Int
minCol = minimum . fmap getCol

maxCol ::(Foldable t, Functor t) => t Coordinate -> Int
maxCol = maximum . fmap getCol

getNorth = modifyY (+ (-1))
getSouth = modifyY (+ 1)
getWest = modifyX (+ (-1))
getEast = modifyX (+ 1)
getNorthEast = getNorth . getEast
getNorthWest = getNorth . getWest
getSouthEast = getSouth . getEast
getSouthWest = getSouth . getWest

-- >>> getAroundCoordinatesIncludingDiagonals (XY (0,0))
-- [XY (0,-1),XY (1,-1),XY (1,0),XY (1,1),XY (0,1),XY (-1,1),XY (-1,0),XY (-1,-1)]
getAroundCoordinatesIncludingDiagonals coordinate = [getNorth, getNorthEast, getEast, getSouthEast, getSouth, getSouthWest, getWest, getNorthWest] <*> [coordinate]

-- >>> getAroundCoordinatesExcludingDiagonals (XY (0,0))
-- [XY (0,-1),XY (1,0),XY (0,1),XY (-1,0)]
-- >>> getAroundCoordinatesExcludingDiagonals (Matrix (0,0))
-- [Matrix (0,-1),Matrix (1,0),Matrix (0,1),Matrix (-1,0)]
getAroundCoordinatesExcludingDiagonals coordinate = [getNorth, getEast, getSouth, getWest] <*> [coordinate]

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance from to = abs (getX from - getX to) + abs (getY from - getY to)


-- >>> normalizeCoordinates id const [XY (-1,-1), XY(2,2)] 
-- [XY (0,0),XY (3,3)]
-- >>> normalizeCoordinates id const [XY (10,10), XY(2,2)] 
-- [XY (8,8),XY (0,0)]
normalizeCoordinates :: (a -> Coordinate) -> (Coordinate -> a -> a) -> [a] -> [a]
normalizeCoordinates coordinateGetter coordinateSetter coordinatesDataList  = do
    let coordinates = map coordinateGetter coordinatesDataList
    let minXPosition = minX coordinates
    let minYPosition = minY coordinates
    map (\coordinatesData -> coordinateSetter (coordinateGetter coordinatesData `subtractCoordinates` XY(minXPosition, minYPosition)) coordinatesData) coordinatesDataList
