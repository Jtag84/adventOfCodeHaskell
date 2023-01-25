module Year2018.Day07 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific, many1)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe ()
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
import Data.Tuple.All (Sel4(sel4), Sel5 (sel5))

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Step = Char
type FinishStep = Char
type BeforeStep = Char

------------ PARSER ------------

inputParser :: Parser (Map.Map BeforeStep [FinishStep])
inputParser = Map.fromListWith (++) <$> many1 stepParser

stepParser :: Parser (BeforeStep, [FinishStep])
stepParser = do
    "Step "
    finishStep <- letter
    " must be finished before step "
    beforeStep <- letter
    " can begin."
    endOfLine <|> endOfInput
    return (beforeStep, [finishStep])


------------ PART A ------------
getAllSteps :: Map.Map BeforeStep [FinishStep] -> Set.Set Step
getAllSteps rules = do
    let values = Set.fromList $ concatMap snd $ Map.assocs rules
    Map.keysSet rules `Set.union` values

findNextSteps :: Map.Map BeforeStep [FinishStep] -> [Step] -> [Step] -> [Step] -> [Step]
findNextSteps _ currentSteps _ [] = currentSteps
findNextSteps rules currentSteps freeToGo remainingSteps = do
    let nextSteps = freeToGo <> filter (all (`elem` currentSteps) . (Map.!) rules) remainingSteps
    let nextStep = minimum nextSteps
    findNextSteps rules (currentSteps <> [nextStep]) (delete nextStep freeToGo) (delete nextStep remainingSteps)

-- Part A:
-- "ADEFKLBVJQWUXCNGORTMYSIHPZ"
-- (0.000100s)
partA rules = do
        let allSteps = getAllSteps rules
        let startSteps = sort . Set.toList $ allSteps Set.\\ Map.keysSet rules
        findNextSteps rules [] startSteps (Set.toList allSteps \\ startSteps)

------------ PART B ------------

calculateTimePartB :: Map.Map BeforeStep [FinishStep] -> ([Step], [Step], [Step], [(Int, Step)], Int) -> ([Step], [Step], [Step], [(Int, Step)], Int)
calculateTimePartB _ state@(_, [], [], [], _) = state
calculateTimePartB rules (currentSteps, readyToGo, remainingSteps, inProgressSteps, time) = do
    let completedSteps = map snd inProgressCompleted
    let newCurrentSteps = currentSteps <> completedSteps
    let nextAvailableSteps = readyToGo <> filter (all (`elem` newCurrentSteps) . (Map.!) rules) remainingSteps
    let nextStepsTaken = take availableWorkers . sort $ nextAvailableSteps
    let nextStepsTakenInProgress = map (\step -> (getStepsTime step, step)) nextStepsTaken
    (newCurrentSteps, (readyToGo \\ completedSteps) \\ nextStepsTaken, remainingSteps \\ nextStepsTaken, stillInProgress <> nextStepsTakenInProgress, time + 1)
    where
        getStepsTime step = fromEnum step - fromEnum 'A' + 61
        (inProgressCompleted, stillInProgress) = break ((> 0) . fst) . sort . decreaseAllFst $ inProgressSteps
        availableWorkers = do
            let availableWorker = 15 - length stillInProgress
            if availableWorker < 0 
                then 0
                else availableWorker

decreaseAllFst :: Num numberToDecrease => [(numberToDecrease, b)] -> [(numberToDecrease, b)]
decreaseAllFst = map (\(numberToDecrease, b) -> (numberToDecrease - 1, b))

-- Part B:
-- 1120
-- (0.001925s)
partB rules = do
        let allSteps = getAllSteps rules
        let startSteps = sort . Set.toList $ allSteps Set.\\ Map.keysSet rules
        (1 +) . sel5 . last . takeWhile (\(_,startSteps,_,inProgress,_) -> (not . null) startSteps || (not . null) inProgress) . iterate (calculateTimePartB rules) $ ([], startSteps, Set.toList allSteps \\ startSteps, [], -1)