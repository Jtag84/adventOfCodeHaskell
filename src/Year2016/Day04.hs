module Year2016.Day04 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, sepBy', inClass, satisfy, digit)
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
import Util.Util (freq)
import Data.Tuple.All (Sel2(sel2))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type EncryptedName = [String]
type SectorId = Int
type Checksum = String

type Room = (EncryptedName, SectorId, Checksum)
------------ PARSER ------------

inputParser :: Parser [Room]
inputParser = many1' roomParser

encryptedNameParser :: Parser EncryptedName
encryptedNameParser =  many1' letter `sepBy'` "-" <* char '-'

sectorIdParser :: Parser SectorId
sectorIdParser = read <$> many1' digit

checksumParser :: Parser Checksum
checksumParser = do
    char '['
    checksum <- many1' letter
    char ']'
    endOfInput <|> endOfLine
    return checksum

roomParser :: Parser Room
roomParser = (,,) <$> encryptedNameParser <*> sectorIdParser <*> checksumParser


------------ PART A ------------

isChecksumValid :: Room -> Bool
isChecksumValid (encryptedNames, _, checksum) = calculateChecksum == checksum
    where
        calculateChecksum = map fst . take 5 . sortBy (compare `on` \(letter, frequency) -> (-frequency, letter)) $ Map.toList (freq (concat encryptedNames))

-- Part A:
-- 158835
-- (0.005681s)
partA = sum . map sel2 . filter isChecksumValid

------------ PART B ------------
-- >>> decryptRoomName (["drxevkzt","jtrmvexvi","ylek","ivrthlzjzkzfe"], 893, "evzkr")
-- (["magnetic","scavenger","hunt","reacquisition"],893,"evzkr")
decryptRoomName :: Room -> Room
decryptRoomName (encryptedNames, sectorId, checksum) = (map (map (U.getNextNthChar sectorId)) encryptedNames, sectorId, checksum)

-- Part B:
-- ... (["northpole","object","storage"],993,"jozmb"), ...
-- (0.005099s)
partB :: [Room] -> [Room]
partB = map decryptRoomName . filter isChecksumValid 
