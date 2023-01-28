module Year2020.Day07 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
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

runDay :: R.Day
runDay = R.runDay rulesParser partA partB

------------ TYPES ------------
type Adjective = String
type Color = String

type Bag = (Adjective, Color)

type NumberOfBags = (Int, Bag)

type LineRule = (Bag, [NumberOfBags])
------------ PARSER ------------

rulesParser :: Parser (Map.Map Bag [NumberOfBags])
rulesParser = Map.fromList <$> many lineRuleParser

bagParser :: Parser Bag
bagParser = do
    adjective <- many letter
    " "
    color <- many letter
    " bags" <|> " bag"
    return (adjective, color)

numberOfBagsParser :: Parser NumberOfBags
numberOfBagsParser = do
    Just number <- toBoundedInteger <$> scientific
    " "
    bag <- bagParser
    return (number, bag)

lineRuleParser :: Parser LineRule
lineRuleParser = do
    bag <- bagParser
    " contain "
    containsBags <- ("no other bags" $> []) <|> (numberOfBagsParser `sepBy` ", ")
    "."
    endOfLine <|> endOfInput
    return (bag, containsBags)

------------ PART A ------------
getBagsThatContainBagDirectly :: Map.Map Bag [NumberOfBags] -> Bag -> [Bag]
getBagsThatContainBagDirectly rules bag = map fst . filter (elem bag . map snd . snd) $ Map.assocs rules

getAllBagsThatContainBag rules bag = nub . sort . concat . takeWhile (not . null) . drop 1 . iterate (concatMap (getBagsThatContainBagDirectly rules)) $ [bag]

-- Part A:
-- 115
-- (0.005452s)
partA rules = length $ getAllBagsThatContainBag rules ("shiny", "gold")

------------ PART B ------------
countAllBagsNeededForBag rules bagToCount = do
    let numberOfBags = rules Map.! bagToCount 
    sum . map (\(number, bag) -> number + number * countAllBagsNeededForBag rules bag) $ numberOfBags

-- Part B:
-- 1250
-- (0.000085s)
partB rules = countAllBagsNeededForBag rules ("shiny", "gold")