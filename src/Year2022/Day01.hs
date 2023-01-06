module Year2022.Day01 (runDay) where

import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, many', scientific, parseOnly, many1')
import Data.List (sort)
import Data.Scientific (Scientific, toBoundedInteger)
import Program.RunDay qualified as R (Day, runDay)
import Data.Maybe (fromMaybe, fromJust)
import Options.Applicative ((<|>))


runDay :: R.Day
runDay = R.runDay rucksacParser partA partB

------------ PARSER ------------
-- >>> parseOnly rucksacParser "123\n345\n4"
-- ProgressCancelledException
rucksacParser :: Parser Rucksac
rucksacParser = many1' elfParser

-- >>> parseOnly elfParser "123\n345\n43"
-- Right [123,345,43]
elfParser :: Parser Elf
elfParser = many1' caloryParser <* (endOfLine <|> endOfInput)

-- >>> parseOnly caloryParser "123\n345"
-- Right 123
caloryParser :: Parser Calory
caloryParser = fromJust . toBoundedInteger <$> scientific <* (endOfLine <|> endOfInput)

------------ TYPES ------------
type Rucksac = [Elf]

type Elf = [Calory]

type Calory = Int

------------ PART A ------------
-- >>> partA [[1,2,3],[3,4]]
-- 7
-- Part A:
-- 67027
-- (0.000037s)
partA :: Rucksac -> Calory
partA = maximum . map sum

------------ PART B ------------
-- Part B:
-- 197291
-- (0.000962s)
partB :: Rucksac -> Calory
partB = sum . take 3 . reverse . sort . map sum
