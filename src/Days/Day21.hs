{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Days.Day21 (runDay) where

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
import Data.Attoparsec.Text as P
import Data.Void
import Data.Text (Text)
import Data.Scientific (toBoundedInteger)
import Options.Applicative ((<|>), Alternative (empty), value)
import Control.Monad.State (State, modify, get, evalState, runState)
import Data.Functor

runDay :: R.Day
runDay = R.runDay monkeysParser partA partB

------------ PARSER ------------
valueParser :: Parser MonkeyOperation 
valueParser = Value . fromJust . toBoundedInteger <$> scientific

monkeyIdParser :: Parser MonkeyId
monkeyIdParser = P.take 4

operatorParser :: Parser Operator
operatorParser = P.choice [
    char '+' $> plus,
    char '-' $> minus,
    char '/' $> dividesBy,
    char '*' $> multipliesBy
    ]

monkeyOperationParser :: Parser MonkeyOperation
monkeyOperationParser = do
    leftMonkeyId <- monkeyIdParser
    " "
    operator <- operatorParser
    " "
    rightMonkeyId <- monkeyIdParser
    return $ Operation leftMonkeyId operator rightMonkeyId

monkeyParser :: Parser (MonkeyId, MonkeyOperation)
monkeyParser = do
    monkey <- monkeyIdParser
    ": "
    operarion <- monkeyOperationParser <|> valueParser
    choice [endOfLine, endOfInput]
    return (monkey, operarion) 

monkeysParser :: Parser Monkeys
monkeysParser = Map.fromList <$> many1' monkeyParser

------------ TYPES ------------
type Monkeys = Map MonkeyId MonkeyOperation

type MonkeyId = Text

data OperatorName = 
    Plus | Minus | DividesBy | MultipliesBy
    deriving(Show,Eq)

data Operator = 
   Operator OperatorName (Int -> Int -> Int)

plus = Operator Plus (+)
minus = Operator Minus (-)
dividesBy = Operator DividesBy div
multipliesBy = Operator MultipliesBy (*)

instance Show Operator where
  show (Operator operatorName f) = show operatorName


data MonkeyOperation where
  Value :: Int -> MonkeyOperation
  Operation:: MonkeyId -> Operator -> MonkeyId -> MonkeyOperation
  deriving(Show)

isOperation :: MonkeyOperation -> Bool
isOperation (Value _) = False
isOperation Operation {} = True

type MonkeyCache = Map MonkeyId Int

------------ PART A ------------
calculate :: MonkeyId -> Monkeys -> State MonkeyCache Int
calculate monkeyId monkeys = do
        inCache <- isInCache
        if inCache 
        then 
            getFromCache
        else do
            result <- case monkeys Map.! monkeyId of
                        Value monkeyValue -> return monkeyValue
                        Operation leftMonkey (Operator name function) rightMonkey -> function <$> calculate leftMonkey monkeys <*> calculate rightMonkey monkeys

            modify (Map.insert monkeyId result)

            return result
    where
        isInCache :: State MonkeyCache Bool
        isInCache = do
            cache <- get
            return $ monkeyId `Map.member` cache

        getFromCache :: State MonkeyCache Int
        getFromCache = do
            cache <- get
            return $ cache Map.! monkeyId

-- Part A:
-- 31017034894002
-- (0.00)
partA :: Monkeys -> Int
partA monkeys = evalState (calculate "root" monkeys) Map.empty

------------ PART B ------------
type UsedIn = Map MonkeyId MonkeyId

getUsedInMap :: Monkeys -> UsedIn
getUsedInMap = Map.fromList  . concatMap usedIn . filter (isOperation . snd) . Map.toList
    where
        usedIn (monkeyId, Operation leftMonkeyId _ rightMonkeyId) = [(leftMonkeyId,monkeyId), (rightMonkeyId, monkeyId)]

solveFor :: MonkeyId -> Monkeys -> UsedIn -> Monkeys
solveFor "root" monkeys usedIn = Map.insert "root" (Value 0) monkeys
solveFor monkeyId monkeys usedIn = do
    let monkeyIdUsedIn = usedIn Map.! monkeyId
    let operation = monkeys Map.! monkeyIdUsedIn
    let solvingOperation = getSolvingOperation monkeyId monkeyIdUsedIn operation
    let monkeysWithoutMonkeyIdUsedIn = Map.delete monkeyIdUsedIn monkeys
    let monkeysWithNewOperation = Map.insert monkeyId solvingOperation monkeysWithoutMonkeyIdUsedIn
    solveFor monkeyIdUsedIn monkeysWithNewOperation usedIn 

getSolvingOperation :: MonkeyId -> MonkeyId -> MonkeyOperation -> MonkeyOperation
getSolvingOperation _ _ value@(Value _ ) = value
getSolvingOperation monkeyIdToSolve "root" (Operation leftId _ rightId) 
    | monkeyIdToSolve == leftId =  Operation "root" plus rightId
    | monkeyIdToSolve == rightId =  Operation "root" plus leftId
    | otherwise = error "id not present in operation"
getSolvingOperation monkeyIdToSolve monkeyIdKey (Operation leftId (Operator Plus _) rightId) 
    | monkeyIdToSolve == leftId =  Operation monkeyIdKey minus rightId
    | monkeyIdToSolve == rightId =  Operation monkeyIdKey minus leftId
    | otherwise = error "id not present in operation"
getSolvingOperation monkeyIdToSolve monkeyIdKey (Operation leftId (Operator Minus _) rightId) 
    | monkeyIdToSolve == leftId =  Operation monkeyIdKey plus rightId
    | monkeyIdToSolve == rightId =  Operation leftId minus monkeyIdKey
    | otherwise = error "id not present in operation"
getSolvingOperation monkeyIdToSolve monkeyIdKey (Operation leftId (Operator DividesBy _) rightId) 
    | monkeyIdToSolve == leftId =  Operation monkeyIdKey multipliesBy rightId
    | monkeyIdToSolve == rightId =  Operation leftId dividesBy monkeyIdKey
    | otherwise = error "id not present in operation"
getSolvingOperation monkeyIdToSolve monkeyIdKey (Operation leftId (Operator MultipliesBy _) rightId) 
    | monkeyIdToSolve == leftId =  Operation monkeyIdKey dividesBy rightId
    | monkeyIdToSolve == rightId =  Operation monkeyIdKey dividesBy leftId
    | otherwise = error "id not present in operation"
    
-- Part B:
-- 3555057453229
-- (0.00)    
partB :: Monkeys -> Int
partB monkeys = do
    let usedIn = getUsedInMap monkeys
    let monkeysSolvingForHumn = solveFor "humn" (Map.delete "humn" monkeys) usedIn
    evalState (calculate "humn" monkeysSolvingForHumn) Map.empty