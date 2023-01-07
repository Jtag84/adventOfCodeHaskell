module Year2015.Day04 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, isEndOfLine, takeTill)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List as L hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Sequence (mapWithIndex)
import Data.Set qualified as Set
import Data.Text as T (Text, pack, unpack)
import Data.Vector qualified as Vec
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U
import qualified Data.ByteString as B
import qualified Crypto.Hash.MD5 as MD5
import Data.Text.Encoding (encodeUtf8)
import Text.Printf (printf)
import Data.Bits
import Data.ByteString.Char8 as BC

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

------------ PARSER ------------

inputParser :: Parser Text
inputParser = takeTill isEndOfLine

------------ PART A ------------

-- Part A:
-- 346386
-- (0.095877s)
partA :: Text -> Int
partA secret = mine5Zeroes (encodeUtf8 secret) 0

mine5Zeroes :: B.ByteString -> Int -> Int
mine5Zeroes secret number = 
    if doesMd5HashStartsWithFiveZeroes . B.unpack . MD5.hash $ secret <> BC.pack (show number)
        then
            number
        else
            mine5Zeroes secret (number + 1)
-- >>> doesMd5HashStartsWithFiveZeroes [0,0,0x04,1,2]
-- True
-- >>> doesMd5HashStartsWithFiveZeroes [0,0,0x14,1,2]
-- False
doesMd5HashStartsWithFiveZeroes md5Hash = do
    let first2Bytes = L.take 2 md5Hash
    let fifthHexaDigit = md5Hash L.!! 2 .&. 0xf0
    first2Bytes == [0,0] && fifthHexaDigit == 0

toHexString :: B.ByteString -> Text
toHexString = B.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""

------------ PART B ------------

doesMd5HashStartsWithSixZeroes md5Hash = do
    let first2Bytes = L.take 3 md5Hash
    first2Bytes == [0,0,0] 


mine6Zeroes :: B.ByteString -> Int -> Int
mine6Zeroes secret number = 
    if doesMd5HashStartsWithSixZeroes . B.unpack . MD5.hash $ secret <> BC.pack (show number)
        then
            number
        else
            mine6Zeroes secret (number + 1) 

-- Part B:
-- 9958218
-- (2.024558s)
partB :: Text -> Int
partB secret = mine6Zeroes (encodeUtf8 secret) 0