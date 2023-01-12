module Year2016.Day05 (runDay) where

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
import Data.Text as T (Text, index, pack, unpack)
import Data.Vector qualified as Vec
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import Year2015.Day04 (mine5Zeroes, toHexString)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.Char qualified as DC
import Data.Containers.ListUtils (nubOrdOn)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

inputParser :: Parser String
inputParser = many1' letter

------------ PART A ------------

getPasswordChar :: String -> (String, Int) -> (String, Int)
getPasswordChar doorId (_ , index) = do
    let (indexThatGeneratesMd5HashStartingWith5Zeroes, md5Hash) = mine5Zeroes (encodeUtf8 (T.pack doorId)) (index+1)
    (T.unpack $ toHexString (B.pack md5Hash), indexThatGeneratesMd5HashStartingWith5Zeroes)

-- Part A:
-- "4543c154"
-- (2.882027s)    
partA doorId = map ((!! 5) . fst) . drop 1 . take 9 . iterate (getPasswordChar doorId) $ ("", 0)

------------ PART B ------------

-- Part B:
-- "1050cbbd"
-- (7.760024s)
partB doorId = map snd . sort . take 8 . nubOrdOn fst . filter isValidPosition . map getCharWithposition . drop 1 . iterate (getPasswordChar doorId) $ ("", 0)

isValidPosition (position, _) = position >=0 && position <=7

getCharWithposition :: (String, Int) -> (Int, Char)
getCharWithposition (md5Hash, _) = (DC.digitToInt $ md5Hash !! 5, md5Hash !! 6)
