module Year2018.Day02 (runDay) where

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
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser [String]
inputParser = many1' (many1' letter <* (endOfInput <|> endOfLine))

------------ PART A ------------

aLetterAppearsExactlyNtimes :: Int -> String -> Bool
aLetterAppearsExactlyNtimes n = any ((==n) . length ) . group . sort 

-- Part A:
-- 8398
-- (0.002373s)
partA boxIds = do
    let numberOfWordsWithExactly2Letters = length $ filter (aLetterAppearsExactlyNtimes 2) boxIds
    let numberOfWordsWithExactly3Letters = length $ filter (aLetterAppearsExactlyNtimes 3) boxIds
    numberOfWordsWithExactly2Letters * numberOfWordsWithExactly3Letters

------------ PART B ------------

getSimilarCharacters :: (String,String) -> String
getSimilarCharacters (id1,id2) = map fst . filter (uncurry (==)) $ zip id1 id2

getAllPairs list = [(x,y) | x <- list, y <- list, x < y]

-- Part B:
-- ["hhvsdkatysmiqjxunezgwcdpr"]
-- (0.016669s)
partB = filter ((==25) . length) . map getSimilarCharacters . getAllPairs