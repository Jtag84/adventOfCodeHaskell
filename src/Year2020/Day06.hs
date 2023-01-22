module Year2020.Day06 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, sepBy1')
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
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [[String]]
inputParser = groupAnswers `sepBy1'` endOfLine

personAnswerParser :: Parser String
personAnswerParser = many1' letter <* (endOfInput <|> endOfLine)

groupAnswers :: Parser [String]
groupAnswers = many1' personAnswerParser

------------ PART A ------------

-- Part A:
-- 6583
-- (0.002367s)
partA = sum . map (length . nub . sort . concat) 

------------ PART B ------------

-- Part B:
-- 3290
-- (0.002358s)
partB = sum . map (\groupAnswers -> length . filter ((== length groupAnswers) . length) . group . sort . concat $ groupAnswers) 