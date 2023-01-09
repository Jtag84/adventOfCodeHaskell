{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal pattern" #-}
module Year2015.Day11 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, inClass)
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

------------ PARSER ------------

inputParser :: Parser String
inputParser = many1' letter <* (endOfInput <|> endOfLine)

------------ PART A ------------

-- >>> has3increasingStraightLetters "abcwwww"
-- True
-- >>> has3increasingStraightLetters "abdwwww"
-- False
-- >>> has3increasingStraightLetters "abdwwxyzww"
-- True
-- >>> has3increasingStraightLetters "abdwwryzaww"
-- False
-- >>> has3increasingStraightLetters "abdwwrrzabww"
-- False
-- >>> has3increasingStraightLetters "abcdffaa"
-- True
has3increasingStraightLetters ('z':xs) = has3increasingStraightLetters xs
has3increasingStraightLetters ('y':'z':xs) = has3increasingStraightLetters xs
has3increasingStraightLetters (a:b:c:xs) = getNextChar a == b && getNextChar b == c || has3increasingStraightLetters (b:c:xs)
has3increasingStraightLetters _ = False

-- >>> hasNoIOL "iol"
-- False
-- >>> hasNoIOL "qweejkjnmgngjhu"
-- True
-- >>> hasNoIOL "abcdffaa"
-- True
hasNoIOL :: String -> Bool
hasNoIOL = not . any (inClass "iol")

-- >>> has2DifferentPairs "aafertggertr"
-- True
-- >>> has2DifferentPairs "aafertaaertr"
-- False
-- >>> has2DifferentPairs "afertaaerzztr"
-- True
has2DifferentPairs :: String -> Bool
has2DifferentPairs = (>= 2) . length . group . sort . concat . filter ((>1) . length) . group

-- >>> isPasswordValid "hijklmmn"
-- False
-- >>> isPasswordValid "abcdffaa"
-- True
isPasswordValid :: String -> Bool
isPasswordValid password = 
           hasNoIOL password
        && has3increasingStraightLetters password
        && has2DifferentPairs password

-- >>> getNextPassword "abcdefgh"
-- "abcdefgi"
-- >>> getNextPassword "abcdefgz"
-- "abcdefha"
-- >>> getNextPassword "abcdefzz"
-- "abcdegaa"
-- >>> getNextPassword "abzzzzzz"
-- "acaaaaaa"
getNextPassword :: String -> String
getNextPassword         "zzzzzzzz"   =                                     "aaaaaaaa"
getNextPassword (a:      "zzzzzzz")  = getNextChar a :                      "aaaaaaa"
getNextPassword (a:b:     "zzzzzz")  = a : getNextChar b :                   "aaaaaa"
getNextPassword (a:b:c:    "zzzzz")  = a : b : getNextChar c :                "aaaaa"
getNextPassword (a:b:c:d:   "zzzz")  = a : b : c : getNextChar d :             "aaaa"
getNextPassword (a:b:c:d:e:  "zzz")  = a : b : c : d : getNextChar e :          "aaa"
getNextPassword (a:b:c:d:e:f: "zz")  = a : b : c : d : e : getNextChar f :       "aa"
getNextPassword (a:b:c:d:e:f:g:"z")  = a : b : c : d : e : f : getNextChar g :    "a"
getNextPassword (a:b:c:d:e:f:g:h:[]) = a : b : c : d : e : f : g : getNextChar h : ""

-- >>> getNextValidPassword 1 "abcdefgh"
-- "abcdffaa"
-- >>> getNextValidPassword 2 "abcdefgh"
-- "abcdffbb"
--
-- >>> getNextValidPassword 1 "ghijklmn"
-- "ghjaabcc"
getNextValidPassword :: Int -> String -> String
getNextValidPassword n = (!! (n - 1)) . filter isPasswordValid . iterate getNextPassword
-- >>> getNextChar 'a'
-- 'b'
-- >>> getNextChar 'z'
-- 'a'
getNextChar :: Char -> Char
getNextChar 'z' = 'a'
getNextChar c = toEnum . (+1) . fromEnum $ c

partA :: String -> String
partA = getNextValidPassword 1

------------ PART B ------------

partB :: String -> String
partB = getNextValidPassword 2