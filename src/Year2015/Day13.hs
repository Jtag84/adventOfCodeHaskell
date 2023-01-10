module Year2015.Day13 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger, scientific)
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

runDay :: R.Day
runDay = R.runDay happinessMapParser partA partB

------------ TYPES ------------

type Person = String

type Hapiness = ((Person, Person), Int)

type HapinessMap = Map.Map (Person, Person) Int

------------ PARSER ------------

personParser :: Parser Person
personParser = many1' letter

happinessParser :: Parser Hapiness
happinessParser = do
    person <- personParser
    " would "
    happinessValue <- P.choice [
                "gain " >> fromJust . toBoundedInteger <$> P.scientific,
                "lose " >> ((-1) *) . fromJust . toBoundedInteger <$> P.scientific
            ]
    " happiness units by sitting next to "
    sittingNextTo <- personParser
    "."
    endOfLine <|> endOfInput
    return ((person, sittingNextTo), happinessValue)

happinessMapParser :: Parser HapinessMap
happinessMapParser = Map.fromList <$> many1' happinessParser

------------ PART A ------------

getAllPersons :: HapinessMap -> Set.Set Person
getAllPersons happinessMap = Set.fromList $ concatMap ((\(left, right) -> [left,right]) . fst) $ Map.toList happinessMap

-- Part A:
-- 733
-- (0.075450s)
partA = maximum . getAllPossibleHappinesses

getAllPossibleHappinesses :: HapinessMap -> [Int]
getAllPossibleHappinesses happinessMap = map (getTotalHappinesses happinessMap) $ permutations . Set.toList . getAllPersons $ happinessMap

getHappinessesForSittingOnRight :: HapinessMap -> [Person] -> [Int]
getHappinessesForSittingOnRight happinessMap (person:sittingNextTo:xs) = happinessMap Map.! (person, sittingNextTo) : getHappinessesForSittingOnRight happinessMap (sittingNextTo:xs)
getHappinessesForSittingOnRight _ _ = []

getTotalHappinesses :: HapinessMap -> [Person] -> Int
getTotalHappinesses happinessMap persons = do
    let personsWrapped = last persons : persons
    let sittingRightHappinesses = getHappinessesForSittingOnRight happinessMap personsWrapped
    let sittingLeftHappinesses = getHappinessesForSittingOnRight happinessMap $ reverse personsWrapped
    sum sittingRightHappinesses + sum sittingLeftHappinesses


------------ PART B ------------

-- Part B:
-- 725
-- (0.783951s)
partB happinessMap= do
    let allPersons = getAllPersons happinessMap
    let happinessForMyselfMap = Map.fromList . concat . Set.toList $ Set.map (\person -> [(("Myself", person), 0), ((person, "Myself"), 0)]) allPersons
    let happinessMapWithMyself = Map.union happinessMap happinessForMyselfMap
    maximum . getAllPossibleHappinesses $ happinessMapWithMyself