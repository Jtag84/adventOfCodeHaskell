module Year2017.Day09 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, anyChar, satisfy, inClass)
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

------------ TYPES ------------

data Stream = 
      Groups [Stream]
    | Garbage String
    | EmptyGroup
    deriving(Eq, Show)

------------ PARSER ------------

inputParser :: Parser Stream
inputParser = groupsParser <* (endOfInput <|> endOfLine)

garbageExclusionParser :: Parser ()
garbageExclusionParser = "!" >> anyChar >> return ()

garbageCharParser :: Parser (Maybe Char)
garbageCharParser = P.choice [
        Just <$> satisfy (inClass "a-z<{},'\""),
        garbageExclusionParser $> Nothing
    ]

garbageParser :: Parser Stream
garbageParser = do
    "<"
    garbageString <- catMaybes <$> many garbageCharParser
    ">"
    return $ Garbage garbageString

groupsParser :: Parser Stream
groupsParser = do
    "{"
    groups <- P.choice [garbageParser, groupsParser] `sepBy` ","
    "}"
    return $ 
        case groups of
            [] -> EmptyGroup
            _ -> Groups groups

------------ PART A ------------
calculateScore :: Int -> Stream -> Int
calculateScore level EmptyGroup = level
calculateScore level (Garbage _) = 0
calculateScore level (Groups groups) = level + (sum . map (calculateScore (level +1 )) $ groups)

-- Part A:
-- 23588
-- (0.000058s)
partA = calculateScore 1

------------ PART B ------------
calculateGarbageSize :: Stream -> Int
calculateGarbageSize EmptyGroup = 0
calculateGarbageSize (Groups groups) = sum . map calculateGarbageSize $ groups
calculateGarbageSize (Garbage garbage) = length garbage

-- Part B:
-- 10045
-- (0.000245s)
partB = calculateGarbageSize