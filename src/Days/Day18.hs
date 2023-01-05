module Days.Day18 where

import Data.List
    ( drop, (\\), group, sort, sortBy, sortOn)
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
import Data.List.GroupBy(groupBy)

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

-- Part A:
-- 4332
-- (0.008813s)
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

-- Part B:
-- 2524
-- (11.474669s)
partB :: CubeCoordinates -> Int
partB cubeCoordinates = do
    let blockedAirPocketCubes = getAllSurroundingPocketAirCubes cubeCoordinates
    calculateUnconnectedSurfaces . Set.toList $ Set.fromList cubeCoordinates `Set.union` blockedAirPocketCubes

-- >>> getAllSurroundingPocketAirCubes2 $ [(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- fromList [(2,2,5)]
-- >>> getAllSurroundingPocketAirCubes2 $ [(2,2,4),(2,2,7),(1,2,5),(1,2,6),(3,2,5),(3,2,6),(2,1,5),(2,1,6),(2,3,5),(2,3,6)]
-- fromList [(2,2,5),(2,2,6)]
-- >>> getAllSurroundingPocketAirCubes2 $ [(2,2,4),(2,2,8),(1,2,5),(1,2,6),(1,2,7),(3,2,5),(3,2,6),(3,2,7),(2,1,5),(2,1,6),(2,1,7),(2,3,5),(2,3,6),(2,3,7)]
-- fromList [(2,2,5),(2,2,6),(2,2,7)]
-- >>> getAllSurroundingPocketAirCubes2 $ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]
-- fromList [(2,2,5)]
getAllSurroundingPocketAirCubes cubeCoordinates = do
    findAirPocketClusters getInitialAirCubes
    where
        findAirPocketClusters currentAirCubes = do
            let airPocketClusters = getClustersWithinLimits currentAirCubes
            let allAirCubesFromPockets = Set.unions airPocketClusters
            let nextSurroundingAirCubes = Set.unions . Set.map getSurroundingPocketAirCubes $ allAirCubesFromPockets
            let nextAirPocketClusters = getClustersWithinLimits . Set.toList $ nextSurroundingAirCubes Set.\\ cubeCoordinateSet `Set.union` allAirCubesFromPockets
            if nextAirPocketClusters == airPocketClusters
                then
                    allAirCubesFromPockets
                else
                    findAirPocketClusters . Set.toList . Set.unions $ nextAirPocketClusters 

        getClustersWithinLimits = filter (all withinCubeCoordinates) . getClusters
        cubeCoordinateSet = Set.fromList cubeCoordinates
        getInitialAirCubes = Set.toList  $ (Set.unions . Set.map getSurroundingPocketAirCubes) cubeCoordinateSet Set.\\ cubeCoordinateSet
        withinCubeCoordinates (x, y, z) = x <= maxX && x >= minX && y <= maxY && y >= minY && z <= maxZ && z >= minZ 
        minX = minimum $ map sel1 cubeCoordinates
        maxX = maximum $ map sel1 cubeCoordinates
        minY = minimum $ map sel2 cubeCoordinates
        maxY = maximum $ map sel2 cubeCoordinates
        minZ = minimum $ map sel3 cubeCoordinates
        maxZ = maximum $ map sel3 cubeCoordinates

-- >>> getSurroundingPocketAirCubes (1,2,4)
-- fromList [(0,2,4),(1,1,4),(1,2,3),(1,2,5),(1,3,4),(2,2,4)]
getSurroundingPocketAirCubes :: CubeCoordinate -> Set CubeCoordinate
getSurroundingPocketAirCubes (x,y,z) = Set.fromList [(x-1,y,z), (x+1,y,z), (x,y-1,z), (x,y+1,z), (x,y,z-1), (x,y,z+1)]

sortByX :: CubeCoordinates -> CubeCoordinates
sortByX  = sortBy (compare `on` sel1)

sortByY :: CubeCoordinates -> CubeCoordinates
sortByY  = sortBy (compare `on` sel2)

sortByZ :: CubeCoordinates -> CubeCoordinates
sortByZ  = sortBy (compare `on` sel3)

-- >>> groupByConnectedOnX [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(4,2,2)]
-- [fromList [(2,1,2)],fromList [(2,1,5)],fromList [(2,2,1)],fromList [(1,2,2),(2,2,2),(3,2,2),(4,2,2)],fromList [(2,2,3)],fromList [(2,2,4)],fromList [(1,2,5)],fromList [(3,2,5)],fromList [(2,2,6)],fromList [(2,3,2)],fromList [(2,3,5)]]
groupByConnectedOnX :: CubeCoordinates -> [Set CubeCoordinate]
groupByConnectedOnX = concatMap (map Set.fromList . groupBy (\(x1,_,_) (x2,_,_) -> abs (x1 - x2) == 1) . sortByX) . getSameYZgroup

-- >>> groupByConnectedOnY [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(4,2,2)]
-- [fromList [(1,2,2)],fromList [(1,2,5)],fromList [(2,2,1)],fromList [(2,1,2),(2,2,2),(2,3,2)],fromList [(2,2,3)],fromList [(2,2,4)],fromList [(2,1,5)],fromList [(2,3,5)],fromList [(2,2,6)],fromList [(3,2,2)],fromList [(3,2,5)],fromList [(4,2,2)]]
groupByConnectedOnY :: CubeCoordinates -> [Set CubeCoordinate]
groupByConnectedOnY = concatMap (map Set.fromList . groupBy (\(_,y1,_) (_,y2,_) -> abs (y1 - y2) == 1) . sortByY) . getSameXZgroup

-- >>> groupByConnectedOnZ [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(4,2,2)]
-- [fromList [(1,2,2)],fromList [(1,2,5)],fromList [(2,1,2)],fromList [(2,1,5)],fromList [(2,2,1),(2,2,2),(2,2,3),(2,2,4)],fromList [(2,2,6)],fromList [(2,3,2)],fromList [(2,3,5)],fromList [(3,2,2)],fromList [(3,2,5)],fromList [(4,2,2)]]
groupByConnectedOnZ :: CubeCoordinates -> [Set CubeCoordinate]
groupByConnectedOnZ = concatMap (map Set.fromList . groupBy (\(_,_,z1) (_,_,z2) -> abs (z1 - z2) == 1) . sortByZ) . getSameXYgroup

-- >>> getClusters [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5),(4,2,2)]
-- [fromList [(1,2,2),(2,1,2),(2,2,1),(2,2,2),(2,2,3),(2,2,4),(2,3,2),(3,2,2),(4,2,2)],fromList [(1,2,5)],fromList [(2,3,5)],fromList [(2,2,6)],fromList [(2,1,5)],fromList [(3,2,5)]]
getClusters :: CubeCoordinates -> [Set CubeCoordinate]
getClusters cubeCoordinates = do
    let connectedOnX = groupByConnectedOnX cubeCoordinates
    let connectedOnY = groupByConnectedOnY cubeCoordinates
    let connectedOnZ = groupByConnectedOnZ cubeCoordinates
    mergeAllConnectedOnAxis (mergeAllConnectedOnAxis connectedOnX connectedOnY) connectedOnZ
    where
        mergeAllConnectedOnAxis [] rightSets = rightSets
        mergeAllConnectedOnAxis (leftSet:leftSets) rightSets = mergeAllConnectedOnAxis leftSets (mergeConnectedOnAxis leftSet rightSets)
        mergeConnectedOnAxis :: Set CubeCoordinate -> [Set CubeCoordinate] -> [Set CubeCoordinate]
        mergeConnectedOnAxis leftSet [] = [leftSet]
        mergeConnectedOnAxis leftSet (rightSet:rightSets) = do
            if any (`Set.member` rightSet) leftSet
                then 
                    mergeConnectedOnAxis (Set.union leftSet rightSet) rightSets
                else
                    mergeConnectedOnAxis leftSet rightSets <> [rightSet]
