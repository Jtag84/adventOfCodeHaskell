module Year2015.Day03 (runDay) where

import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText)
import Data.Functor (($>))
import Data.List as L (sort, group, nub)
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinate (getEast, getNorth, getSouth, getWest, Coordinate (XY))
import Data.Foldable
import Data.Sequence (mapWithIndex)

runDay :: R.Day
runDay = R.runDay directionsParser partA partB

------------ TYPES ------------
data Direction = North | East | South | West
  deriving (Show, Eq)
------------ PARSER ------------

directionParser :: Parser Direction
directionParser = choice [
      char '>' $> East,
      char '^' $> North,
      char '<' $> West,
      char 'v' $> South
    ]

directionsParser :: Parser [Direction]
directionsParser = many1' directionParser <* (endOfLine <|> endOfInput)

------------ PART A ------------
-- Part A:
-- 2572
-- (0.004029s)
partA = getNumberOfUniqueCoordinates . getCoordinates

getNumberOfUniqueCoordinates = length . group . sort

getCoordinates = foldl' (\coordinates direction -> getCoordinate (head coordinates) direction : coordinates) [XY (0,0)] 

getCoordinate coordinate East = getEast coordinate
getCoordinate coordinate North = getNorth coordinate
getCoordinate coordinate South = getSouth coordinate
getCoordinate coordinate West = getWest coordinate

------------ PART B ------------
-- Part B:
-- 2631
-- (0.004214s)
partB directions = do
  let directionsWithIndex = zip [1..] directions
  let santaDirections = map snd . filter (odd . fst) $ directionsWithIndex
  let robotSantaDirections = map snd . filter (even . fst) $ directionsWithIndex
  getNumberOfUniqueCoordinates $ getCoordinates santaDirections <> getCoordinates robotSantaDirections

