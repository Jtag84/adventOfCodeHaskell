module Year2022.Day23 (runDay) where

{- ORMOLU_DISABLE -}
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
import Util.Coordinate 
import Data.Text (Text)
import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay elfParser partA partB

------------ PARSER ------------
elfParser  :: Parser (Set Elf)
elfParser = do
    Set.fromList . snd . snd <$> runScanner (XY (0,0) ,[]) scanPredicate 
    where
        scanPredicate :: (Coordinate, [Elf]) -> Char -> Maybe (Coordinate, [Elf])
        scanPredicate (currentCoordinate, elfs) currentChar = 
            case currentChar of
                '#'  -> Just (nextOnLine currentCoordinate, currentCoordinate:elfs)
                '.'  -> Just (nextOnLine currentCoordinate, elfs)
                '\n' -> Just (nextLine currentCoordinate, elfs)
                _    -> Nothing

        nextOnLine (XY (x, y)) = XY (x+1, y)
        nextLine (XY (x, y)) = XY (0, y+1)

------------ TYPES ------------
type Elf = Coordinate

------------ PART A ------------

-- >>> getNorthBound (XY (1,1))
-- [XY (0,0),XY (1,0),XY (2,0)]
getNorthBound coordinate = Vec.fromList [getNorthWest, getNorth, getNorthEast] <*> pure coordinate
-- >>> getWestBound (XY (1,1))
-- [XY (0,0),XY (0,1),XY (0,2)]
getWestBound coordinate = Vec.fromList [getNorthWest, getWest, getSouthWest] <*> pure coordinate
-- >>> getSouthBound (XY (1,1))
-- [XY (0,2),XY (1,2),XY (2,2)]
getSouthBound coordinate = Vec.fromList [getSouthWest, getSouth, getSouthEast] <*> pure coordinate
-- >>> getEastBound (XY (1,1))
-- [XY (2,2),XY (2,1),XY (2,0)]
getEastBound coordinate = Vec.fromList [getSouthEast, getEast, getNorthEast] <*> pure coordinate

-- >>> getAllAround (XY (3,4))
-- [XY (2,3),XY (3,3),XY (4,3),XY (4,5),XY (4,4),XY (2,4),XY (3,5),XY (2,5)]
getAllAround :: Coordinate -> Vector Coordinate
getAllAround coordinate = Vec.fromList [getNorthWest, getNorth, getNorthEast, getSouthEast, getEast, getWest, getSouth, getSouthWest] <*> pure coordinate

-- >>> isIsolated (Set.fromList [XY (3,4),XY (3,6),XY (3,7)]) (XY (3,4))
-- True
-- >>> isIsolated (Set.fromList [XY (3,4),XY (3,6),XY (3,7)]) (XY (3,6))
-- False
isIsolated elves coordinate = Vec.all (`Set.notMember` elves) $ getAllAround coordinate 

type ProposedMove = Elf

-- >>> getProposedMove (Set.fromList [XY (3,4),XY (3,6),XY (3,7)]) predicatesOrdered (XY (3,4))
-- (XY (3,4),XY (3,4))
-- >>> getProposedMove (Set.fromList [XY (3,4),XY (3,6),XY (3,7)]) predicatesOrdered (XY (3,6))
-- (XY (3,6),XY (3,5))
-- >>> getProposedMove (Set.fromList [XY (3,4),XY (3,6),XY (3,7)]) predicatesOrdered (XY (3,5))
-- (XY (3,5),XY (2,5))
getProposedMove :: Set Elf -> [Coordinate -> Vector Coordinate] -> Elf -> (Elf, ProposedMove)
getProposedMove _ [] elf = (elf, elf)
getProposedMove elves (predicateCoordinate:predicateCoordinates) elf
    | isIsolated elves elf = (elf, elf)
    | Vec.any (`Set.member` elves) getCoordinatePredicate = getProposedMove elves predicateCoordinates elf
    | otherwise = (elf, getNextMove)
    where
        getCoordinatePredicate = predicateCoordinate elf
        getNextMove = getCoordinatePredicate Vec.! 1

getAllNextMoves :: Set Elf -> [Coordinate -> Vector Coordinate] -> Set (Elf, ProposedMove)
getAllNextMoves elves predicates = Set.map (getProposedMove elves predicates) elves

predicatesOrdered = [getNorthBound, getSouthBound, getWestBound, getEastBound]

executeOneRound :: Set Elf -> [Coordinate -> Vector Coordinate] -> Set Elf
executeOneRound elves predicates = do
    let allNextMoves = getAllNextMoves elves predicates
    let groupedProposedMoves = groupBy (\a b -> snd a == snd b) $ sortBy (compare `on` snd) $ Set.toList allNextMoves
    Set.fromList $ concatMap getFinalMove groupedProposedMoves
    where
        -- only one in the list so no duplicate proposed move
        getFinalMove [element] = [snd element]
        getFinalMove duplicatedProposedMoves = map fst duplicatedProposedMoves


executeNRounds :: Int -> Set Elf -> [Coordinate -> Vector Coordinate] -> Set Elf
executeNRounds 0 elves _ = elves
executeNRounds numberOfRounds elves predicates@(fistPredicates:otherPredicates) = do
    let newElves = executeOneRound elves predicates
    executeNRounds (numberOfRounds - 1) newElves (otherPredicates <> [fistPredicates])

calculateEmptyGroundTiles :: Set Elf -> Int
calculateEmptyGroundTiles elves = do
    (maxX - minX + 1) * (maxY - minY + 1) - Set.size elves
    where
        minX = getX $ minimumBy (compare `on` getX) elves
        maxX = getX $ maximumBy (compare `on` getX) elves
        minY = getY $ minimumBy (compare `on` getY) elves
        maxY = getY $ maximumBy (compare `on` getY) elves


-- Part A:
-- 4070
-- (0.070276s)
partA :: Set Elf -> Int
partA elves = calculateEmptyGroundTiles $ executeNRounds 10 elves predicatesOrdered

------------ PART B ------------
findRoundWithNoMovingElves :: Int -> Set Elf -> [Coordinate -> Vector Coordinate] -> Int
findRoundWithNoMovingElves n elves predicates@(firstPredicates:otherPredicates) = do
    let newElves = executeOneRound elves predicates
    if elves == newElves
        then
            n + 1
        else 
            findRoundWithNoMovingElves (n+1) newElves (otherPredicates <> [firstPredicates])
        
-- Part B:
-- 881
-- (5.090950s)
partB :: Set Elf -> Int
partB elves = findRoundWithNoMovingElves 0 elves predicatesOrdered
