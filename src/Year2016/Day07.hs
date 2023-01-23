{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Year2016.Day07 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, isEndOfLine, takeTill, anyChar, manyTill')
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
inputParser = many1' $ many1' (letter <|> char '[' <|> char ']') <* (endOfInput <|> endOfLine)

------------ PART A ------------

-- >>> supportsTls 0 False "abba[mnop]qrst"
-- True
-- >>> supportsTls 0 False "abcd[bddb]xyyx"
-- False
-- >>> supportsTls 0 False "xyyx[bddb]xyyx"
-- False
-- >>> supportsTls 0 False "aaaa[qwer]tyui"
-- False
-- >>> supportsTls 0 False "ioxxoj[asdfgh]zxcvbn"
-- True
supportsTls bracketsCount foundTlsSupport ('[':xs) = supportsTls (bracketsCount + 1) foundTlsSupport xs
supportsTls bracketsCount foundTlsSupport (']':xs) = supportsTls (bracketsCount - 1) foundTlsSupport xs
supportsTls bracketsCount foundTlsSupport (a:b:c:d:xs)
    | a==d && b == c && a /= b = 
        if withinBrackets
            then False
            else supportsTls bracketsCount True (b:c:d:xs)
    | otherwise = supportsTls bracketsCount foundTlsSupport (b:c:d:xs)
    where
        withinBrackets = bracketsCount > 0

supportsTls _ foundTlsSupport _ = foundTlsSupport

-- Part A:
-- 118
-- (0.001031s)
partA = length . filter (supportsTls 0 False)

------------ PART B ------------
-- >>> supportsSsl 0 Set.empty Set.empty "aba[bab]xyz"
-- True
-- >>> supportsSsl 0 Set.empty Set.empty "xyx[xyx]xyx"
-- False
-- >>> supportsSsl 0 Set.empty Set.empty "aaa[kek]eke"
-- True
-- >>> supportsSsl 0 Set.empty Set.empty "zazbz[bzb]cdb"
-- True
supportsSsl :: Int -> Set.Set String -> Set.Set String -> String -> Bool
supportsSsl bracketsCount abaFoundSet babFoundSet ('[':xs) = supportsSsl (bracketsCount + 1) abaFoundSet babFoundSet xs
supportsSsl bracketsCount abaFoundSet babFoundSet (']':xs) = supportsSsl (bracketsCount - 1) abaFoundSet babFoundSet xs
supportsSsl bracketsCount abaFoundSet babFoundSet (a:b:c:xs)
    | isPattern = 
        if withinBrackets
            then if Set.member [b,a,b] abaFoundSet
                then True
                else supportsSsl bracketsCount abaFoundSet (Set.insert [a,b,c] babFoundSet) (b:c:xs)
            else if Set.member [b,a,b] babFoundSet
                then True
                else supportsSsl bracketsCount (Set.insert [a,b,c] abaFoundSet) babFoundSet (b:c:xs)
    | otherwise = supportsSsl bracketsCount abaFoundSet babFoundSet (b:c:xs)
    where
        withinBrackets = bracketsCount > 0
        isPattern = a == c && a /= b
supportsSsl _ _ _ _ = False

-- Part B:
-- 260
-- (0.001209s)
partB = length . filter (supportsSsl 0 Set.empty Set.empty)