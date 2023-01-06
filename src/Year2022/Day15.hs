{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day15 where

import Data.Attoparsec.Text
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Scientific (toBoundedInteger)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Util.Coordinate
import Util.Range
import Util.Util qualified as U
import Options.Applicative ((<|>))
import Control.Concurrent ()
import Control.Parallel.Strategies

runDay :: R.Day
runDay = R.runDay reportParser partA partB

------------ TYPES ------------
type Sensor = Coordinate

type Beacon = Coordinate

type ReportRow = (Sensor, Beacon)

type Report = [ReportRow]

------------ PARSER ------------
-- >>> parseOnly coordinateParser "x=2, y=18"
-- Right (XY (2,18))
coordinateParser :: Parser Coordinate
coordinateParser = do
  "x="
  Just x <- toBoundedInteger <$> scientific
  ", y="
  Just y <- toBoundedInteger <$> scientific
  return $ XY (x, y)

-- >>> parseOnly sensorParser "Sensor at x=2, y=18"
-- Right (XY (2,18))
sensorParser :: Parser Sensor
sensorParser = "Sensor at " *> coordinateParser

beaconParser :: Parser Beacon
beaconParser = "closest beacon is at " *> coordinateParser

reportRowParser :: Parser ReportRow
reportRowParser = do
  sensor <- sensorParser
  ": "
  beacon <- beaconParser
  endOfLine <|> endOfInput
  return (sensor, beacon)

reportParser :: Parser Report
reportParser = many' reportRowParser

------------ PART A ------------
-- >>> getSensorManhattanDistancePairs [(XY (8,7), XY (2,10))]
-- [(XY (8,7),9)]
getSensorManhattanDistancePairs :: Report -> [(Sensor, Int)]
getSensorManhattanDistancePairs = map getSensorManhattanDistancePair
  where
    getSensorManhattanDistancePair :: ReportRow -> (Sensor, Int)
    getSensorManhattanDistancePair (sensor, beacon) = (sensor, abs (getX sensor - getX beacon) + abs (getY sensor - getY beacon))

-- >>> getSensorCoverageRangesForRow 10 [(XY (8,7),9)]
-- [Range (2,14) Nothing]
getSensorCoverageRangesForRow :: Int -> [(Sensor, Int)] -> [Range]
getSensorCoverageRangesForRow rowNumberY = map getSensorCoverage
  where
    getSensorCoverage sensorManhattanPair@(sensor, manhattanDistance)
      | xSpread sensorManhattanPair > 0 = Range (getX sensor - xSpread sensorManhattanPair, getX sensor + xSpread sensorManhattanPair) Nothing
      | otherwise = EmptyRange
    xSpread (sensor, manhattanDistance) = manhattanDistance - abs (getY sensor - rowNumberY)

countBeaconsOnRow :: Int -> Report -> Int
countBeaconsOnRow rowNumberY = length . filter ((==) rowNumberY . getY) . nub . map snd

-- partA :: Report -> Int
-- partA = countBeaconsOnRow 2000000 
-- partA :: Report -> Range
-- partA = mergeRanges . getSensorCoverageRangesForRow 2000000 . getSensorManhattanDistancePairs 
partA :: Report -> Int
partA report = sensorCoverageOnRow - countBeaconsOnRow 2000000 report
  where
    sensorCoverageOnRow = countRange . mergeRanges . getSensorCoverageRangesForRow 2000000 . getSensorManhattanDistancePairs $ report

------------ PART B ------------
partB :: Report -> [(Int, [Range])]
partB report = getInnerHoleRangesWithIndex 
  where
    sensorManhattanPairs = getSensorManhattanDistancePairs report
    getAllRowRanges = withStrategy (parBuffer 40 rseq) $ map (mergeRanges . flip getSensorCoverageRangesForRow sensorManhattanPairs) [0 .. 4000000]
    getInnerHoleRangesWithIndex = filter (not . null . snd) $ zip [0..] $ map inverseWithinRange getAllRowRanges
