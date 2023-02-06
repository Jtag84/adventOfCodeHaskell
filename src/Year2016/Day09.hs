module Year2016.Day09 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, anyChar, inClass, satisfy)
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
import Data.Sequence (mapWithIndex, replicate)
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
runDay = R.runDay compressedStringParser partA partB

------------ TYPES ------------
type NumberOfChars = Int
type Repetition = Int

data CompressedData =
      Marker NumberOfChars Repetition
    | Data Char
    deriving(Eq, Show)

type CompressedString = [CompressedData]

------------ PARSER ------------

compressedStringParser :: Parser CompressedString
compressedStringParser = many compressedDataParser <* (endOfInput <|> endOfLine)

markerParser :: Parser CompressedData
markerParser = do
    "("
    Just numberOfChars <- toBoundedInteger <$> P.scientific
    "x"
    Just numberOfRepetition <- toBoundedInteger <$> P.scientific
    ")"
    return $ Marker numberOfChars numberOfRepetition

compressedDataParser :: Parser CompressedData
compressedDataParser = P.choice [
        markerParser,
        Data <$> P.satisfy (inClass "a-zA-Z0-9()")
    ]

------------ PART A ------------
calculateLengthPart1 :: CompressedString -> Int
calculateLengthPart1 [] = 0
calculateLengthPart1 (Data _:xs) = 1  + calculateLengthPart1 xs
calculateLengthPart1 (Marker numberOfChars repetition:xs) = numberOfChars * repetition + calculateLengthPart1 (skipCompressedData numberOfChars xs)

skipCompressedData :: Int -> CompressedString -> CompressedString
skipCompressedData 0 compressedString = compressedString
skipCompressedData numberOfCharsToSkip (Data _:xs) = skipCompressedData (numberOfCharsToSkip - 1) xs
skipCompressedData numberOfCharsToSkip (marker@(Marker numberOfChars repetition):xs) = skipCompressedData (numberOfCharsToSkip - getCompressedDataLength marker) xs

getCompressedDataLength (Data _) = 1
getCompressedDataLength marker = length $ markerToString marker

markerToString (Marker numberOfChars repetition) = "(" ++ show numberOfChars ++ "x" ++ show repetition ++ ")"

-- Part A:
-- 110346
-- (0.000210s)
partA = calculateLengthPart1 

------------ PART B ------------
-- >>> calculateLengthPart2 [Marker 6 1,Marker 1 3,Data 'A']
-- 3
calculateLengthPart2 :: CompressedString -> Int
calculateLengthPart2 [] = 0
calculateLengthPart2 (Data _:xs) = 1  + calculateLengthPart2 xs
calculateLengthPart2 (Marker numberOfChars repetition:xs) = repetition * calculateLengthPart2 (takeCompressedData numberOfChars xs) + calculateLengthPart2 (skipCompressedData numberOfChars xs)

takeCompressedData :: Int -> CompressedString -> CompressedString
takeCompressedData 0 _ = []
takeCompressedData numberOfCharsToTake (dataC@(Data _):xs) = dataC:takeCompressedData (numberOfCharsToTake - 1) xs
takeCompressedData numberOfCharsToTake (marker@(Marker numberOfChars repetition):xs) = marker:takeCompressedData (numberOfCharsToTake - getCompressedDataLength marker) xs

-- Part B:
-- 10774309173
-- (0.002408s)
partB = calculateLengthPart2