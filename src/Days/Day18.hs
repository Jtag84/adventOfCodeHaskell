module Days.Day18 where

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
    ( many', endOfInput, scientific, endOfLine, Parser )
import Data.Void
import Data.Scientific (toBoundedInteger)
import Options.Applicative ((<|>), Alternative (empty))
import Data.Tuple.Select (Sel3(sel3), Sel1 (sel1), Sel2 (sel2))
import Data.Function (on)

runDay :: R.Day
runDay = R.runDay cubeCoordinatesParser partA partB

------------ PARSER ------------
cubeCoordinateParser :: Parser CubeCoordinate
cubeCoordinateParser = do
    Just x <- toBoundedInteger <$> scientific
    ","
    Just y <- toBoundedInteger <$> scientific
    ","
    Just z <- toBoundedInteger <$> scientific
    endOfLine <|> endOfInput
    return (x,y,z)

cubeCoordinatesParser :: Parser CubeCoordinates
cubeCoordinatesParser = many' cubeCoordinateParser

------------ TYPES ------------
type X = Int
type Y = Int
type Z = Int
type CubeCoordinate = (X,Y,Z)
type CubeCoordinates = [CubeCoordinate]

------------ PART A ------------
partA :: CubeCoordinates -> Int
partA = calculateUnconnectedSurfaces

-- >>> map getSortedX . getSameYZgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- >>> countConnectedFacesForEachGroupAndSum $ map getSortedX . getSameYZgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- >>> map getSortedY . getSameXZgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- >>> countConnectedFacesForEachGroupAndSum $ map getSortedY . getSameXZgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- >>> map getSortedZ . getSameXYgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- >>> countConnectedFacesForEachGroupAndSum $ map getSortedZ . getSameXYgroup $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [[2],[2],[2],[1,2,3],[2],[2],[1,3],[2],[2],[2]]
-- 4
-- [[2],[2],[2],[1,2,3],[2],[2],[1,3],[2],[2],[2]]
-- 4
-- [[2,5],[2,5],[1,2,3,4,6],[2,5],[2,5]]
-- 6
-- >>> calculateUnconnectedSurfaces [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- 64
calculateUnconnectedSurfaces :: CubeCoordinates -> Int
calculateUnconnectedSurfaces cubeCoordinates = do
    let totalSurfaces = getTotalSurfaces cubeCoordinates
    let sortedXofYZgroups = map getSortedX . getSameYZgroup $ cubeCoordinates
    let sortedYofXZgroups = map getSortedY . getSameXZgroup $ cubeCoordinates
    let sortedZofXYgroups = map getSortedZ . getSameXYgroup $ cubeCoordinates
    totalSurfaces - countConnectedFacesForEachGroupAndSum sortedXofYZgroups - countConnectedFacesForEachGroupAndSum sortedYofXZgroups - countConnectedFacesForEachGroupAndSum sortedZofXYgroups
    
-- >>> countConnectedFacesForEachGroupAndSum [[1,2,3],[2],[2],[2],[2],[2],[2],[1,3],[2],[2]]
-- 4
countConnectedFacesForEachGroupAndSum :: [[Int]] -> Int
countConnectedFacesForEachGroupAndSum = sum . map countConnectedFaces

-- >>> countConnectedFaces [2,3,4]
-- 4
countConnectedFaces :: [Int] -> Int
countConnectedFaces a = 2 * (length . filter ( == 1) $ zipWith (-) (Data.List.drop 1 a) a)

-- >>> getTotalSurfaces [[1,2,3],[2],[2],[2],[2],[2],[2],[1,3],[2],[2]]
-- 78
getTotalSurfaces :: CubeCoordinates -> Int
getTotalSurfaces = (6 *) . length

getSortedX :: CubeCoordinates -> [Int]
getSortedX = sort . map sel1

getSortedY :: CubeCoordinates -> [Int]
getSortedY = sort . map sel2

getSortedZ :: CubeCoordinates -> [Int]
getSortedZ = sort . map sel3

-- >>> getSameXYgroup [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [[(1,2,2),(1,2,5)],[(2,1,2),(2,1,5)],[(2,2,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6)],[(2,3,2),(2,3,5)],[(3,2,2),(3,2,5)]]
getSameXYgroup :: CubeCoordinates -> [CubeCoordinates]
getSameXYgroup = groupBy (\(x1,y1,_) (x2,y2,_) -> (x1 == x2) && (y1 == y2)) . sortOn (\(x,y,_) -> (x,y))

-- >>> getSameXZgroup [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [[(1,2,2)],[(1,2,5)],[(2,2,1)],[(2,2,2),(2,1,2),(2,3,2)],[(2,2,3)],[(2,2,4)],[(2,1,5),(2,3,5)],[(2,2,6)],[(3,2,2)],[(3,2,5)]]
getSameXZgroup :: CubeCoordinates -> [CubeCoordinates]
getSameXZgroup = groupBy (\(x1,_,z1) (x2,_,z2) -> (x1 == x2) && (z1 == z2)) . sortOn (\(x,_,z) -> (x,z))

-- >>> getSameYZgroup [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [[(2,2,2),(1,2,2),(3,2,2)],[(2,1,2)],[(2,3,2)],[(2,2,1)],[(2,2,3)],[(2,2,4)],[(2,2,6)],[(1,2,5),(3,2,5)],[(2,1,5)],[(2,3,5)]]
getSameYZgroup :: CubeCoordinates -> [CubeCoordinates]
getSameYZgroup = groupBy (\(_,y1,z1) (_,y2,z2) -> (z1 == z2) && (y1 == y2)) . sortOn (\(_,y,z) -> (y,z))

------------ PART B ------------
-- >>> partB [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- 58
partB :: CubeCoordinates -> Int
partB cubeCoordinates = do
    let allUnconnectedCubeSurfaces = calculateUnconnectedSurfaces cubeCoordinates 
    let allAirPocketSurfaces = (6 *) . length $ findBlockedAirPocket cubeCoordinates
    let allUnconnectedAirPoacketSurfaces = calculateUnconnectedSurfaces $ findBlockedAirPocket cubeCoordinates
    allUnconnectedCubeSurfaces - allUnconnectedAirPoacketSurfaces
-- >>> findBlockedAirPocket [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [(2,2,5)]
findBlockedAirPocket :: CubeCoordinates -> CubeCoordinates
findBlockedAirPocket = map (!! 0) . filter ((6 ==) . length) . group . sort . getAllSurroundingPocketAirCubes

-- >>> getAllSurroundingPocketAirCubes . getAllSurroundingPocketAirCubes $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- [(-1,2,2),(1,2,2),(0,1,2),(0,3,2),(0,2,1),(0,2,3),(0,1,2),(2,1,2),(1,0,2),(1,2,2),(1,1,1),(1,1,3),(0,3,2),(2,3,2),(1,2,2),(1,4,2),(1,3,1),(1,3,3),(0,2,1),(2,2,1),(1,1,1),(1,3,1),(1,2,0),(1,2,2),(0,2,3),(1,1,3),(1,3,3),(1,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(3,2,2),(5,2,2),(4,1,2),(4,3,2),(4,2,1),(4,2,3),(2,1,2),(4,1,2),(3,0,2),(3,2,2),(3,1,1),(3,1,3),(2,3,2),(4,3,2),(3,2,2),(3,4,2),(3,3,1),(3,3,3),(2,2,1),(4,2,1),(3,1,1),(3,3,1),(3,2,0),(3,2,2),(2,2,3),(4,2,3),(3,1,3),(3,3,3),(3,2,2),(0,1,2),(2,1,2),(1,0,2),(1,2,2),(1,1,1),(1,1,3),(2,1,2),(4,1,2),(3,0,2),(3,2,2),(3,1,1),(3,1,3),(1,0,2),(3,0,2),(2,-1,2),(2,1,2),(2,0,1),(2,0,3),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(1,1,1),(3,1,1),(2,0,1),(2,2,1),(2,1,0),(2,1,2),(1,1,3),(3,1,3),(2,0,3),(2,2,3),(2,1,2),(0,3,2),(2,3,2),(1,2,2),(1,4,2),(1,3,1),(1,3,3),(2,3,2),(4,3,2),(3,2,2),(3,4,2),(3,3,1),(3,3,3),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(1,4,2),(3,4,2),(2,3,2),(2,5,2),(2,4,1),(2,4,3),(1,3,1),(3,3,1),(2,2,1),(2,4,1),(2,3,0),(2,3,2),(1,3,3),(3,3,3),(2,2,3),(2,4,3),(2,3,2),(0,2,1),(2,2,1),(1,1,1),(1,3,1),(1,2,0),(1,2,2),(2,2,1),(4,2,1),(3,1,1),(3,3,1),(3,2,0),(3,2,2),(1,1,1),(3,1,1),(2,0,1),(2,2,1),(2,1,0),(2,1,2),(1,3,1),(3,3,1),(2,2,1),(2,4,1),(2,3,0),(2,3,2),(1,2,0),(3,2,0),(2,1,0),(2,3,0),(2,2,-1),(2,2,1),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(0,2,3),(2,2,3),(1,1,3),(1,3,3),(1,2,2),(2,2,3),(4,2,3),(3,1,3),(3,3,3),(3,2,2),(1,1,3),(3,1,3),(2,0,3),(2,2,3),(2,1,2),(1,3,3),(3,3,3),(2,2,3),(2,4,3),(2,3,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(0,2,4),(2,2,4),(1,1,4),(1,3,4),(1,2,5),(2,2,4),(4,2,4),(3,1,4),(3,3,4),(3,2,5),(1,1,4),(3,1,4),(2,0,4),(2,2,4),(2,1,5),(1,3,4),(3,3,4),(2,2,4),(2,4,4),(2,3,5),(2,2,4),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(0,2,6),(2,2,6),(1,1,6),(1,3,6),(1,2,5),(1,2,7),(2,2,6),(4,2,6),(3,1,6),(3,3,6),(3,2,5),(3,2,7),(1,1,6),(3,1,6),(2,0,6),(2,2,6),(2,1,5),(2,1,7),(1,3,6),(3,3,6),(2,2,6),(2,4,6),(2,3,5),(2,3,7),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(1,2,7),(3,2,7),(2,1,7),(2,3,7),(2,2,6),(2,2,8),(-1,2,5),(1,2,5),(0,1,5),(0,3,5),(0,2,4),(0,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(0,1,5),(2,1,5),(1,0,5),(1,2,5),(1,1,4),(1,1,6),(0,3,5),(2,3,5),(1,2,5),(1,4,5),(1,3,4),(1,3,6),(0,2,4),(2,2,4),(1,1,4),(1,3,4),(1,2,3),(1,2,5),(0,2,6),(2,2,6),(1,1,6),(1,3,6),(1,2,5),(1,2,7),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(3,2,5),(5,2,5),(4,1,5),(4,3,5),(4,2,4),(4,2,6),(2,1,5),(4,1,5),(3,0,5),(3,2,5),(3,1,4),(3,1,6),(2,3,5),(4,3,5),(3,2,5),(3,4,5),(3,3,4),(3,3,6),(2,2,4),(4,2,4),(3,1,4),(3,3,4),(3,2,3),(3,2,5),(2,2,6),(4,2,6),(3,1,6),(3,3,6),(3,2,5),(3,2,7),(0,1,5),(2,1,5),(1,0,5),(1,2,5),(1,1,4),(1,1,6),(2,1,5),(4,1,5),(3,0,5),(3,2,5),(3,1,4),(3,1,6),(1,0,5),(3,0,5),(2,-1,5),(2,1,5),(2,0,4),(2,0,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(1,1,4),(3,1,4),(2,0,4),(2,2,4),(2,1,3),(2,1,5),(1,1,6),(3,1,6),(2,0,6),(2,2,6),(2,1,5),(2,1,7),(0,3,5),(2,3,5),(1,2,5),(1,4,5),(1,3,4),(1,3,6),(2,3,5),(4,3,5),(3,2,5),(3,4,5),(3,3,4),(3,3,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(2,2,4),(2,2,6),(1,4,5),(3,4,5),(2,3,5),(2,5,5),(2,4,4),(2,4,6),(1,3,4),(3,3,4),(2,2,4),(2,4,4),(2,3,3),(2,3,5),(1,3,6),(3,3,6),(2,2,6),(2,4,6),(2,3,5),(2,3,7)]
getAllSurroundingPocketAirCubes :: CubeCoordinates -> CubeCoordinates
getAllSurroundingPocketAirCubes cubeCoordinates = concatMap getSurroundingPocketAirCubes cubeCoordinates \\ cubeCoordinates

-- >>> getSurroundingPocketAirCubes (1,2,2)
-- [(0,2,2),(2,2,2),(1,1,2),(1,3,2),(1,2,1),(1,2,3)]
getSurroundingPocketAirCubes :: CubeCoordinate -> CubeCoordinates
getSurroundingPocketAirCubes (x,y,z) = [(x-1,y,z), (x+1,y,z), (x,y-1,z), (x,y+1,z), (x,y,z-1), (x,y,z+1)]
