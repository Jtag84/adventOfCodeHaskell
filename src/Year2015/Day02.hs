module Year2015.Day02 (runDay) where

import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText)
import Data.Functor (($>))
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text)
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay (many1' dimensionsParser) partA partB

------------ TYPES ------------
type Length = Int
type Width = Int
type Height = Int

type Dimensions = (Length, Width, Height)
------------ PARSER ------------

dimensionsParser :: Parser Dimensions
dimensionsParser = do
    Just length <- toBoundedInteger <$> scientific
    char 'x'
    Just width <- toBoundedInteger <$> scientific
    char 'x'
    Just height <- toBoundedInteger <$> scientific
    endOfLine <|> endOfInput
    return (length, width, height)

------------ PART A ------------
-- Part A:
-- 1598415
-- (0.000037s)
partA = sum . map getWrapperSurfaceNeeded

getWrapperSurfaceNeeded (length,width,height) = 2 * lengthWidthSurface + 2 * lengthHeightSurface + 2 * widthHeigthSurface + minimum [lengthWidthSurface, lengthHeightSurface, widthHeigthSurface]
  where
    lengthWidthSurface = length * width
    lengthHeightSurface = length * height
    widthHeigthSurface = width * height 

------------ PART B ------------
-- Part B:
-- 3812909
-- (0.000578s)
partB = sum . map (\dimensions -> getSmallestPerimeter dimensions + getBowLength dimensions)

getSmallestPerimeter (length,width,height) = (2 *) . sum . take 2 . sort $ [length,width,height]
getBowLength (length,width,height) = length * width * height