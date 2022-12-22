module Days.Day01 (runDay) where

import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, many', scientific, parseOnly)
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
rucksacParser = many' elfParser

-- >>> parseOnly elfParser "123\n345\n43"
-- Right [123,345,43]
elfParser :: Parser Elf
elfParser = many' caloryParser 

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
partA :: Rucksac -> Calory
partA = maximum . map sum

------------ PART B ------------
partB :: Rucksac -> Calory
partB = sum . take 3 . reverse . sort . map sum
