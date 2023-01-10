module Year2015.Day12 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText)
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
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as JKM

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Aeson.KeyMap as JKM

runDay :: R.Day
runDay = R.runDay takeText partA partB

------------ PART A ------------

-- Part A:
-- 111754
-- (0.004435s)
partA = sum . getNumbers . fromJust . J.decodeStrict . encodeUtf8

getNumbers :: J.Value -> [Int]
getNumbers (J.Number number) = [(fromJust . toBoundedInteger) number]
getNumbers (J.Array vector) = (concat . Vec.toList . Vec.map getNumbers) vector
getNumbers (J.Object object) = concatMap (getNumbers . snd) $ JKM.toList object
getNumbers _ = []

------------ PART B ------------

-- >>> getNumbersWithoutRed $ J.Number 4
-- [4]
-- >>> getNumbersWithoutRed $ J.Array (Vec.fromList ["qwe", J.Number 2])
-- [2]
-- >>> getNumbersWithoutRed $ J.Object (JKM.fromList [("qwe", J.Null), ("n", J.Number 2)])
-- [2]
-- >>> getNumbersWithoutRed $ J.Object (JKM.fromList [("qwe", J.String "red"), ("n", J.Number 2)])
-- []
getNumbersWithoutRed :: J.Value -> [Int]
getNumbersWithoutRed (J.Number number) = [(fromJust . toBoundedInteger) number]
getNumbersWithoutRed (J.Array vector) = (concat . Vec.toList . Vec.map getNumbersWithoutRed) vector
getNumbersWithoutRed (J.Object object) = do
    let values = map snd $ JKM.toList object
    if "red" `elem` values
        then
            []
        else 
            concatMap getNumbersWithoutRed values
getNumbersWithoutRed _ = []

-- Part B:
-- 65402
-- (0.003103s)
partB = sum . getNumbersWithoutRed . fromJust . J.decodeStrict . encodeUtf8
