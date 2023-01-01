module Days.Day25 where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Options.Applicative ((<|>), Alternative (empty))

runDay :: R.Day
runDay = R.runDay snafuListParser partA partB

------------ PARSER ------------
snafuParser :: Parser Int
snafuParser = do
    snafuDigits <- map snafuDigitToInt . reverse <$> snafuCharParser
    return $ sum $ zipWith (*) snafuDigits (map (5^) [0..])

snafuCharParser :: Parser [Char]
snafuCharParser = many1' (satisfy (inClass "-012=")) <* (endOfLine <|> endOfInput)

snafuDigitToInt :: Char -> Int
snafuDigitToInt '0' = 0
snafuDigitToInt '1' = 1
snafuDigitToInt '2' = 2
snafuDigitToInt '-' = -1
snafuDigitToInt '=' = -2
snafuDigitToInt x = error $ "unknow character " ++ show x

snafuListParser :: Parser [Int]
snafuListParser = many1' snafuParser <* endOfInput

intToSnafu :: Int -> [Char]
intToSnafu 0 = ""
intToSnafu number = do
    let (division, remainder) = quotRem number 5
    if remainder > 2 
        then
            do
                let snafuRemainder = case remainder of
                                        3 -> "="
                                        4 -> "-"
                                        _ -> error "don't know what to do for remainder " ++ show remainder
                
                intToSnafu (division + 1) <> snafuRemainder
        else
            intToSnafu division <> show remainder

------------ PART A ------------

-- Part A:
-- "20=022=21--=2--12=-2"
-- (0.000008s)
partA :: [Int] -> [Char]
partA = intToSnafu . sum

------------ PART B ------------
partB :: [Int] -> [Int]
partB = error "Not implemented yet!"
