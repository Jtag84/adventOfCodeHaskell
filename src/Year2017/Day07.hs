module Year2017.Day07 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, many1)
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
import Data.Tuple.All (Sel1(sel1))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Name = String
type Weight = Int
type FromName = Maybe Name
type Holds = [Name]

type FromWeightHolds = (FromName, Weight, Holds)
------------ PARSER ------------

inputParser :: Parser (Map.Map Name FromWeightHolds)
inputParser = do
    lines <- many1 lineParser
    let fromMap = Map.fromList . concatMap (\(name,_,holds) -> map (,name) holds) $ lines
    return $ Map.fromList . map (\(name, weight, holds) -> (name, (fromMap Map.!? name, weight, holds))) $ lines

lineParser :: Parser (Name, Weight, Holds)
lineParser = do
    name <- nameParser
    " "
    weight <- weightParser
    P.choice [
            (endOfLine <|> endOfInput) $> (name, weight, []),
            " -> " >> ((name, weight,) <$> holdsParser) <* (endOfLine <|> endOfInput)
        ] 

nameParser :: Parser Name
nameParser = many1' letter

weightParser :: Parser Weight
weightParser = "(" *> (fromJust . toBoundedInteger <$> scientific ) <* ")"

holdsParser :: Parser Holds
holdsParser = nameParser `sepBy` ", "


------------ PART A ------------
findBase :: Map.Map Name FromWeightHolds -> Name
findBase = Set.elemAt 0 . Map.keysSet . fst . Map.partition (isNothing . sel1) 

-- Part A:
-- "veboyvy"
-- (0.002871s)
partA = findBase

------------ PART B ------------
findBranchesWeight :: Map.Map Name FromWeightHolds -> Name -> [(Name, Weight)]
findBranchesWeight towers name = do
    let (_, weight, branches) = towers Map.! name
    let branchesWeights = map (\branch -> (branch, sum . map snd . findBranchesWeight towers $ branch)) branches
    (name, weight) : branchesWeights

findInvalidWeight :: Map.Map Name FromWeightHolds -> [(Name, Weight)] -> [(Name, Weight)]
findInvalidWeight towers nameWeights = do
    let wrongNameWeight = concat . filter ((== 1) . length) . groupBy (\a b -> snd a == snd b) . sortBy (compare `on` snd) . tail $ nameWeights
    case wrongNameWeight of
        [] -> nameWeights
        [(name, _)] -> do
            let nextNameWeights = findBranchesWeight towers name
            if null nextNameWeights
                then nameWeights
                else findInvalidWeight towers nextNameWeights

-- Part B:
-- 749
-- (0.000641s)
partB towers = do
    let base = findBase towers
    let baseBranchesWeight = findBranchesWeight towers base
    let invalidWeightBranch = findInvalidWeight towers baseBranchesWeight
    let weightDiscrepancy = foldr1 (-) . map (snd . head) . sortBy (compare `on` length) . groupBy (\a b -> snd a == snd b) . sortBy (compare `on` snd) . tail $ baseBranchesWeight
    (snd . head $ invalidWeightBranch) - weightDiscrepancy