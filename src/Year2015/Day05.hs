module Year2015.Day05 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, isEndOfLine, takeTill, letter)
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
import qualified Data.Text as T

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

------------ PARSER ------------

inputParser :: Parser [String]
inputParser = many1' wordParser <* endOfInput

wordParser :: Parser String
wordParser = many1' letter <* (endOfInput <|> endOfLine) 

------------ PART A ------------

-- Part A:
-- 258
-- (0.003251s)
partA = length . getNiceStrings
   
getNiceStrings =
              filter has3Vowels 
            . filter atLeastOneLetterTwiceInArow 
            . filter doesntContainExclusion

-- >>> has3Vowels "abcd"
-- False
-- >>> has3Vowels "aaa"
-- True
-- >>> has3Vowels "abcdef"
-- False
-- >>> has3Vowels "oui"
-- True
has3Vowels = (>=3) . length . filter (== True) . map (`elem` ['a','e','i','o','u'])

-- >>> atLeastOneLetterTwiceInArow "asdfg"
-- False
-- >>> atLeastOneLetterTwiceInArow "asddfg"
-- True
atLeastOneLetterTwiceInArow :: [Char] -> Bool
atLeastOneLetterTwiceInArow = any ((>= 2) . length) . group

-- >>> doesntContainExclusion "aaa"
-- True
-- >>> doesntContainExclusion "aab"
-- False
-- >>> doesntContainExclusion "asdwqabweqwe"
-- False
doesntContainExclusion word = notElem True $ map (not . T.null . snd . flip T.breakOn (T.pack word)) ["ab", "cd", "pq", "xy"]

------------ PART B ------------

-- >>> hasOneLetterInBetween "weqweqwer"
-- False
-- >>> hasOneLetterInBetween "weqweqwqer"
-- True
-- >>> hasOneLetterInBetween "aba"
-- True
hasOneLetterInBetween (a:b:c:xs)
    | a == c = True
    | otherwise = hasOneLetterInBetween (b:c:xs)
hasOneLetterInBetween _ = False

-- >>> aPairAppearsTwice "asddefg"
-- False
-- >>> aPairAppearsTwice "wasddasefg"
-- True
aPairAppearsTwice (a:b:xs) =
        (not . T.null . snd $ T.pack [a,b] `T.breakOn` T.pack xs)
            || aPairAppearsTwice (b:xs)
aPairAppearsTwice _ = False

-- Part B:
-- 53
-- (0.000849s)
partB = 
    length 
    . filter aPairAppearsTwice 
    . filter hasOneLetterInBetween
