module Year2018.Day04 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, takeTill)
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
import Data.Time (LocalTime (..), parseTimeM, defaultTimeLocale, diffLocalTime, TimeOfDay (todMin))
import Data.Time.Clock
import qualified Data.Text as T

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type GuardId = Int

data Event = 
      StartsShift GuardId
    | FallsAsleep
    | WakesUp
    deriving (Eq, Show)

type Record = (LocalTime, Event)

type MinuteTime = Int

------------ PARSER ------------

inputParser :: Parser [Record]
inputParser = many' recordParser

timeParser :: Parser LocalTime
timeParser = do
    "["
    timeString <- takeTill (==']')
    "]"
    parseTimeM True defaultTimeLocale "%F %R" $ T.unpack timeString

eventParser :: Parser Event
eventParser =
    P.choice [
      "Guard #" *> (StartsShift . fromJust . toBoundedInteger <$> scientific) <* " begins shift",
      "falls asleep" $> FallsAsleep,
      "wakes up" $> WakesUp
    ] <* (endOfInput <|> endOfLine)

recordParser :: Parser Record
recordParser = do
    time <- timeParser
    " "
    event <- eventParser
    return (time, event)

------------ PART A ------------

calculateHoursAsleep :: GuardId -> Maybe LocalTime -> Map.Map GuardId [MinuteTime] -> [Record] -> Map.Map GuardId [MinuteTime]
calculateHoursAsleep _ _ guardAsleepTimeMap ((_, StartsShift newGuardShift):records) = calculateHoursAsleep newGuardShift Nothing guardAsleepTimeMap records
calculateHoursAsleep currentId maybeStartToFallAsleepTime guardAsleepTimeMap ((currentRecordTime, FallsAsleep):records) = 
    calculateHoursAsleep currentId (Just currentRecordTime) guardAsleepTimeMap records
calculateHoursAsleep currentId (Just startToFallAsleepTime) guardAsleepTimeMap ((currentRecordTime, WakesUp):records) = do
    let newGuardAsleepTimeMap = Map.alter addTimeAsleep currentId guardAsleepTimeMap
    calculateHoursAsleep currentId Nothing newGuardAsleepTimeMap records
    where
        addTimeAsleep :: Maybe [MinuteTime] -> Maybe [MinuteTime]
        addTimeAsleep (Just currentMinuteList) = Just $ currentMinuteList <> toMinuteList startToFallAsleepTime currentRecordTime 
        addTimeAsleep Nothing = Just $ toMinuteList startToFallAsleepTime currentRecordTime
calculateHoursAsleep _ _ guardAsleepTimeMap [] = guardAsleepTimeMap

toMinuteList :: LocalTime -> LocalTime -> [Int]
toMinuteList start end = [(todMin . localTimeOfDay) start ..  (todMin . localTimeOfDay) end - 1]

getMostAsleepGuard :: Map.Map GuardId [MinuteTime] -> (GuardId, [MinuteTime])
getMostAsleepGuard = maximumBy (compare `on` length . snd) . Map.toList

getMostAsleepMinutes :: [MinuteTime] -> [MinuteTime]
getMostAsleepMinutes = maximumBy (compare `on` length) . group . sort

getMostAsleepMinute :: [MinuteTime] -> MinuteTime
getMostAsleepMinute = head . getMostAsleepMinutes

-- Part A:
-- 65489
-- (0.001075s)
partA records = do
    let ((_, StartsShift firstGuardId):remainingRecords) = sortBy (compare `on` fst) records
    let guardsAsleepMap = calculateHoursAsleep firstGuardId Nothing Map.empty remainingRecords
    let (mostAsleepGuardId, minutes) = getMostAsleepGuard guardsAsleepMap
    mostAsleepGuardId * getMostAsleepMinute minutes

------------ PART B ------------
getGuardMostAsleepOnASpecificMinute ::  Map.Map GuardId [MinuteTime] -> (GuardId, [MinuteTime])
getGuardMostAsleepOnASpecificMinute guardAsleepTimeMap = 
    maximumBy (compare `on` length . snd) . Map.toList $ Map.map getMostAsleepMinutes guardAsleepTimeMap

-- Part B:
-- 3852
-- (0.001414s)
partB records = do
    let ((_, StartsShift firstGuardId):remainingRecords) = sortBy (compare `on` fst) records
    let guardsAsleepMap = calculateHoursAsleep firstGuardId Nothing Map.empty remainingRecords
    let (mostAsleepGuardId, minutes) = getGuardMostAsleepOnASpecificMinute guardsAsleepMap
    mostAsleepGuardId * head minutes