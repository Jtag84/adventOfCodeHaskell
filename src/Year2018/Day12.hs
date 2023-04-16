module Year2018.Day12 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser,anyChar, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, many1)
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
import Util.Util (UnShow(UnShow))

runDay :: R.Day
runDay = R.runDay potsAndRulesParser partA partB

------------ TYPES ------------

data Pot = Plant | NoPlant
    deriving(Eq, Ord, Show)

type Pots = Map.Map Int Pot

type Rule = ([Pot], Pot)

type Rules = Map.Map [Pot] Pot

------------ PARSER ------------

potParser :: Parser Pot
potParser = P.choice [
        "#" $> Plant,
        "." $> NoPlant
    ]

potsParser :: Parser Pots
potsParser = do
    "initial state: "
    Map.fromList . zip [0,1..] <$> P.many1' potParser <* endOfLine

ruleParser :: Parser Rule
ruleParser = do
    from <- P.many' potParser
    " => "
    to <- potParser
    endOfLine <|> endOfInput
    return (from,to)

rulesParser :: Parser Rules
rulesParser = Map.fromList <$> P.many' ruleParser

potsAndRulesParser :: Parser (Pots, Rules)
potsAndRulesParser = do
    pots <- potsParser
    endOfLine
    rules <- rulesParser
    return (pots, rules)
    
------------ PART A ------------
runOneGeneration :: (Pots, Rules) -> (Pots, Rules)
runOneGeneration (pots, rules) = do
    (Map.fromList $ map applyRules indexes, rules)
    where
        indexes = do
            let currentIndexes = Map.keys pots
            minimum currentIndexes - 1:maximum currentIndexes +1:currentIndexes
        applyRules :: Int -> (Int, Pot)
        applyRules index = do
            let indexes = [(+(-2)),(+(-1)),(+0),(+1),(+2)] <*> [index]
            let from = map (\index -> Map.findWithDefault NoPlant index pots) indexes
            (index, Map.findWithDefault NoPlant from rules)

calculateScore = sum . map fst . filter ((==Plant) . snd) . Map.toList . fst 

-- Part A:
-- 3059
-- (0.001088s)
partA = calculateScore . last . take 21 . iterate runOneGeneration

------------ PART B ------------
-- Part B:
-- 3650000001776
-- (0.187209s)
partB potsAndRules = do
    let scores = map calculateScore . take 501 . iterate runOneGeneration $ potsAndRules
    let lastDiff = last $ zipWith (-) scores (tail scores)
    last scores - lastDiff * (50000000000 - 500)
