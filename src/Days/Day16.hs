module Days.Day16 where

import Data.List ( take, maximumBy, minimumBy, subsequences )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes, fromJust, isJust )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text as P
    ( many', sepBy', endOfInput, scientific, endOfLine, take, Parser )
import Data.Void ()
import Data.Text ( Text )
import Options.Applicative ((<|>), Alternative (empty))
import Data.Scientific (toBoundedInteger)
import Control.Monad.State
    ( modify, evalState, MonadState(get), State ) 
import Data.Function (on)
import Data.Tuple.All (Sel1(sel1), Sel3 (sel3))
import Safe (headDef)
import Control.Parallel.Strategies (withStrategy, parBuffer, rseq, parList, rpar)
import Data.Either (fromLeft, lefts, rights)
import Data.Foldable (foldlM)


runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type ValveId = Text

type NextValveIds = [ValveId]

type Path = [ValveId]

type FlowRate = Int
type FlowRates = [FlowRate]

type Valve = (ValveId, (FlowRate, NextValveIds))

type Cave = Map ValveId (FlowRate, NextValveIds)

type CacheFromToValve = Map (ValveId, ValveId) Path

------------ PARSER ------------
inputParser :: Parser Cave
inputParser = Map.fromList <$> many' valveParser

valveIdParser :: Parser ValveId
valveIdParser = P.take 2 

nextValvesParser :: Parser NextValveIds
nextValvesParser = do
    "tunnels lead to valves " <|> "tunnel leads to valve "
    valveIdParser `sepBy'` ", " <* (endOfLine <|> endOfInput)

valveParser :: Parser Valve
valveParser = do
    "Valve "
    valveId <- valveIdParser
    " has flow rate="
    Just flowRate <- toBoundedInteger <$> scientific
    "; "
    nextValves <- nextValvesParser
    return (valveId, (flowRate, nextValves))

------------ PART A ------------

-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [] ["AA"] "DD") Map.empty
-- Just ([0,0],["DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [0,0] ["DD","AA"] "BB") Map.empty
-- Just ([20,20,20,0,0],["BB","CC","DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [33,33,33,33,20,20,20,0,0,0] ["JJ","II","AA","BB","CC","DD","AA"] "HH") Map.empty
-- Just ([54,54,54,54,54,54,54,54,33,33,33,33,20,20,20,0,0,0],["HH","GG","FF","EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [0] ["AA"] "BB") Map.empty
-- Just ([0,0,0],["BB","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [13,0,0] ["CC","BB","AA"] "DD") Map.empty
-- Just ([15,15,13,0,0],["DD","CC","BB","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [20,20,20,0,0] ["BB","CC","DD","AA"] "JJ") Map.empty
-- Just ([33,33,33,33,20,20,20,0,0],["JJ","II","AA","BB","CC","DD","AA"])
getTotalFlowRateFromPathToValveId :: Cave -> FlowRates -> Path -> ValveId -> State CacheFromToValve (Maybe (FlowRates, Path))
getTotalFlowRateFromPathToValveId cave currentFlowRates currentPath toValveId = do
    maybeNewPath <- getFastestPath cave currentValveId toValveId
    case maybeNewPath of
        Nothing -> return Nothing
        Just newPath -> do
            let newPathSize = length newPath
            let newFlowRate = lastMinuteFlowRate + newFlow
            let newFlowRates = replicate newPathSize newFlowRate <> currentFlowRates
            return $ Just (newFlowRates, newPath <> tail currentPath)
    where
        currentValveId = head currentPath
        newFlow = sel1 $ cave Map.! currentValveId
        lastMinuteFlowRate = headDef 0 currentFlowRates

-- >>> evalState (getFastestPath (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) "AA" "DD") Map.empty
-- Just ["DD","AA"]
getFastestPath :: Cave -> ValveId -> ValveId -> State CacheFromToValve (Maybe Path)
getFastestPath cave from to = do
        inCache <- isInCache
        if inCache 
        then 
            Just <$> getFromCache
        else do
            bestPath <- getBestPath cave from to Set.empty [] 
            if isJust bestPath 
            then
                do
                modify (Map.insert (from, to) (fromJust bestPath))
                return bestPath
            else
                return bestPath
    where
        isInCache :: State CacheFromToValve Bool
        isInCache = do
            cache <- get
            return $ (from, to) `Map.member` cache

        getFromCache :: State CacheFromToValve Path
        getFromCache = do
            cache <- get
            return $ cache Map.! (from, to)

-- >>> evalState (getBestPath (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) "AA" "GG" Set.empty []) Map.empty
-- Just ["GG","FF","EE","DD","AA"]
getBestPath :: Cave -> ValveId -> ValveId -> Set ValveId -> Path -> State CacheFromToValve (Maybe Path)
getBestPath cave from to alreadyVisited path
    | from `Set.member` alreadyVisited = return Nothing
    | from == to = return $ Just (from:path)
    | otherwise = do
        let nextValveIds = snd $ cave Map.! from 
        let newAlreadyVisited = Set.insert from alreadyVisited
        paths <- mapM (\nextValve -> getBestPath cave nextValve to newAlreadyVisited (from:path)) nextValveIds
        let justPaths = filter isJust paths
        if not $ null justPaths
        then
            return $ minimumBy (compare `on` fmap length) justPaths
        else 
            return Nothing

-- >>> evalState (getBestPathToMaxPressure (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 26 ([], ["AA"]) (Set.fromList ["DD","EE","HH"])) Map.empty
-- (943,[45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,42,42,42,42,20,20,20,20,20,0,0],["EE","FF","GG","HH","GG","FF","EE","DD","AA"])
-- >>> evalState (getBestPathToMaxPressure (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 26 ([], ["AA"]) (Set.fromList ["CC","BB","JJ"])) Map.empty
-- (764,[36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,34,34,21,21,21,21,0,0,0],["CC","BB","AA","II","JJ","II","AA"])
-- >>> evalState (getBestPathToMaxPressure (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 ([], ["AA"]) (Set.fromList ["CC","DD","BB","EE","HH","JJ"])) Map.empty
-- (1651,[81,81,81,81,81,81,79,79,79,76,76,76,76,54,54,54,54,54,54,54,54,33,33,33,33,20,20,20,0,0],["CC","DD","EE","FF","GG","HH","GG","FF","EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"])
getBestPathToMaxPressure :: Cave -> Int -> (FlowRates, Path) -> Set ValveId -> State CacheFromToValve (FlowRate, FlowRates, Path)
getBestPathToMaxPressure cave minutes currentFlowRatesPath@(currentFlowRates, currentPath) valvesToOpen
    | null valvesToOpen = return $ finalizeFlowRatePath currentFlowRatesPath
    | otherwise = do
        newFlowRatesPaths <- getBestPathToNextValveToOpen cave minutes currentFlowRatesPath valvesToOpen
        let (newFlowRatesPathsWithoutFinalLength, newFlowRatesPathsWithFinalLength) = break ((minutes <=) . length . snd) newFlowRatesPaths
        let finalizedFlowRatesPathsWithFinalLength = map finalizeFlowRatePath newFlowRatesPathsWithFinalLength
        if not $ null newFlowRatesPathsWithoutFinalLength 
            then
                do
                bestFlowRatePathsForNonFinalizedLength <- mapM getAllBestPaths newFlowRatesPathsWithoutFinalLength
                let allFinalFlowRatePaths = bestFlowRatePathsForNonFinalizedLength <> finalizedFlowRatesPathsWithFinalLength
        
                return $ maximumBy (compare `on` sel1) allFinalFlowRatePaths
            else
                return $ maximumBy (compare `on` sel1) finalizedFlowRatesPathsWithFinalLength                
    where
        finalizeFlowRatePath :: (FlowRates, Path) -> (FlowRate, FlowRates, Path)
        finalizeFlowRatePath (tofinalizedFlowRate, toFinalizedPath) = do
            let lastValveId = head toFinalizedPath
            let lastFlow = getFlowRate cave lastValveId + headDef 0 tofinalizedFlowRate
            let finalFlowRates = drop (length tofinalizedFlowRate - minutes) $ replicate (minutes - length tofinalizedFlowRate) lastFlow <> tofinalizedFlowRate
            (sum finalFlowRates, finalFlowRates, toFinalizedPath)

        getAllBestPaths newFlowRatesPath@(_, newPath) = do
                let newValvesToOpen = Set.delete (head newPath) valvesToOpen 
                getBestPathToMaxPressure cave minutes newFlowRatesPath newValvesToOpen

-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 26 ([], ["AA"]) (Set.fromList ["CC","BB","JJ"])) Map.empty
-- [([0,0],["BB","AA"]),([0,0,0],["CC","DD","AA"]),([0,0,0],["JJ","II","AA"])]
-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 26 ([], ["AA"]) (Set.fromList ["CC","BB","JJ"])) Map.empty
-- [([0,0],["BB","AA"]),([0,0,0],["CC","DD","AA"]),([0,0,0],["JJ","II","AA"])]
-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 ([], ["AA"]) (Set.fromList ["CC","DD","BB","EE","HH","JJ"])) Map.empty
-- [([0,0],["BB","AA"]),([0,0,0],["CC","DD","AA"]),([0,0],["DD","AA"]),([0,0,0],["EE","DD","AA"]),([0,0,0,0,0,0],["HH","GG","FF","EE","DD","AA"]),([0,0,0],["JJ","II","AA"])]
-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 ([0,0],["DD","AA"]) (Set.fromList ["CC","BB","EE","HH", "JJ"])) Map.empty
-- [([20,20,20,0,0],["BB","CC","DD","AA"]),([20,20,0,0],["CC","DD","AA"]),([20,20,0,0],["EE","DD","AA"]),([20,20,20,20,20,0,0],["HH","GG","FF","EE","DD","AA"]),([20,20,20,20,0,0],["JJ","II","AA","DD","AA"])]
-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 ([20,20,20,0,0],["BB","CC","DD","AA"]) (Set.fromList ["CC","JJ","EE","HH"])) Map.empty
-- [([33,33,20,20,20,0,0],["CC","BB","CC","DD","AA"]),([33,33,33,33,20,20,20,0,0],["EE","DD","CC","BB","CC","DD","AA"]),([33,33,33,33,33,33,33,20,20,20,0,0],["HH","GG","FF","EE","DD","CC","BB","CC","DD","AA"]),([33,33,33,33,20,20,20,0,0],["JJ","II","AA","BB","CC","DD","AA"])]
-- >>> evalState (getBestPathToNextValveToOpen (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 ([33,33,33,33,20,20,20,0,0],["JJ","II","AA","BB","CC","DD","AA"]) (Set.fromList ["CC","EE","HH"])) Map.empty
-- [([54,54,54,54,54,33,33,33,33,20,20,20,0,0],["CC","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"]),([54,54,54,54,54,33,33,33,33,20,20,20,0,0],["EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"]),([54,54,54,54,54,54,54,54,33,33,33,33,20,20,20,0,0],["HH","GG","FF","EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"])]
getBestPathToNextValveToOpen :: Cave -> Int -> (FlowRates, Path) -> Set ValveId -> State CacheFromToValve [(FlowRates, Path)]
getBestPathToNextValveToOpen cave minutes (currentFlowRates, currentPath) valvesToOpen = do
   maybeFlowRatePaths <- mapM (getTotalFlowRateFromPathToValveId cave currentFlowRates currentPath) $ Set.toList valvesToOpen
   return $ catMaybes maybeFlowRatePaths

compareFlowRatePaths :: Cave -> Int -> (a, Path) -> (a, Path) -> Ordering
compareFlowRatePaths cave minutes (_, leftPath) (_, rightPath) = do
    let maxPathLenth = min minutes (max (length leftPath) (length rightPath) )
    let leftLastFlowRate = getFlowRate cave $ head leftPath
    let rightLastFlowRate = getFlowRate cave $ head rightPath
    let leftPotentialFlowRate = leftLastFlowRate + leftLastFlowRate * (maxPathLenth - length leftPath)
    let rightPotentialFlowRate = rightLastFlowRate + rightLastFlowRate * (maxPathLenth - length rightPath)
    compare leftPotentialFlowRate rightPotentialFlowRate


getFlowRate :: Cave -> ValveId -> FlowRate
getFlowRate cave = fst . (Map.!) cave

allValvesThatCanBeOpened :: Map ValveId (FlowRate, b) -> Set ValveId
allValvesThatCanBeOpened = Set.fromList . map fst . filter ((0 <) . fst . snd ) . Map.toList 

-- Part A:
-- (1559,[104,104,104,90,90,90,67,67,67,58,58,58,58,58,58,58,58,58,58,33,33,33,33,16,16,16,16,0,0,0],["XC","CB","JH","MI","QI","OE","SH","KT","OH","AK","OH","KT","SH","OE","JT","RA","DS","ME","VG","IC","ZZ","EK","GB","EK","ZZ","IC","ZB","TX","KM","WR","AA"])
-- (1.37)
partA :: Cave -> (FlowRate, FlowRates, Path)
partA cave = evalState (getBestPathToMaxPressure cave 30 ([], ["AA"]) (allValvesThatCanBeOpened cave)) Map.empty

------------ PART B ------------

-- Part B:
-- (2191,[165,159,148,148,148,148,148,148,103,103,103,103,74,74,74,74,65,65,65,20,20,20,16,0,0,0],(["BV","JN","BS","IC","ZZ","EK","GB","EK","ZZ","IC","ZB","TX","KM","JF","SD","CN","SD","JF","KM","WR","AA"],["XC","CB","JH","CT","NA","GL","NA","CT","JH","MI","QI","OE","SH","KT","IT","NT","AO","ML","AA"]))
-- (186.65)
partB :: Cave -> (FlowRate, FlowRates, (Path, Path))
partB cave = evalState (getBestFlowRateWithElephant cave 26 "AA" (allValvesThatCanBeOpened cave)) Map.empty

getBestFlowRateWithElephant :: Cave -> Int -> ValveId -> Set ValveId -> State CacheFromToValve (FlowRate, FlowRates, (Path, Path))
getBestFlowRateWithElephant cave initialMinutes initialValveId allValvesThatCanBeOpened = do
    let allSubsequeces = subsequences $ Set.toList allValvesThatCanBeOpened
    let halfSubsequences = Data.List.take (round (fromIntegral (length allSubsequeces) / 2)) allSubsequeces
    allFlows <- mapM (getBestFlowRateWithElephantWithValves . Set.fromList) halfSubsequences
    return $ maxFlow $ withStrategy (parBuffer 40 rseq) allFlows
    where
        getBestFlowRateWithElephantWithValves :: Set ValveId -> State CacheFromToValve (FlowRate, FlowRates, (Path, Path))
        getBestFlowRateWithElephantWithValves valvesToOpenForPerson = do
            let valvesToOpenForElephant = allValvesThatCanBeOpened Set.\\ valvesToOpenForPerson
            (totalFlowRatePerson, flowRatesPerson, pathPerson) <- getBestPathToMaxPressure cave initialMinutes ([], ["AA"]) valvesToOpenForPerson
            (totalFlowRateElephant, flowRatesElephant, pathElephant) <-  getBestPathToMaxPressure cave initialMinutes ([], ["AA"]) valvesToOpenForElephant
            return (totalFlowRateElephant + totalFlowRatePerson, zipWith (+) flowRatesPerson flowRatesElephant , (pathPerson, pathElephant))

maxFlow = maximumBy (compare `on` sel1)