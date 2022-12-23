module Days.Day16 where

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
import Data.Text ( Text )
import Options.Applicative ((<|>), Alternative (empty))
import Data.Scientific (toBoundedInteger)
import Control.Monad.State 
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

-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [0] ["AA"] "DD") Map.empty
-- ([0,0,0],["DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [0,0,0] ["DD","AA"] "BB") Map.empty
-- ([20,20,20,0,0,0],["BB","CC","DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [33,33,33,33,20,20,20,0,0,0] ["JJ","II","AA","BB","CC","DD","AA"] "HH") Map.empty
-- ([54,54,54,54,54,54,54,54,33,33,33,33,20,20,20,0,0,0],["HH","GG","FF","EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [0] ["AA"] "BB") Map.empty
-- ([0,0,0],["BB","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [13,0,0] ["CC","BB","AA"] "DD") Map.empty
-- ([15,15,13,0,0],["DD","CC","BB","AA"])
-- >>> evalState (getTotalFlowRateFromPathToValveId (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) [20,20,20,0,0] ["BB","CC","DD","AA"] "JJ") Map.empty
-- ([33,33,33,33,20,20,20,0,0],["JJ","II","AA","BB","CC","DD","AA"])
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
-- ["DD","AA"]
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
    -- | length path > 1 = return Nothing
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

-- >>> evalState (getBestFlowRate (Map.fromList [("AA",(0,["DD","II","BB"])),("BB",(13,["CC","AA"])),("CC",(2,["DD","BB"])),("DD",(20,["CC","AA","EE"])),("EE",(3,["FF","DD"])),("FF",(0,["EE","GG"])),("GG",(0,["FF","HH"])),("HH",(22,["GG"])),("II",(0,["AA","JJ"])),("JJ",(21,["II"]))]) 30 "AA" (Set.fromList ["CC","DD","BB","EE","HH","JJ"])) Map.empty
-- (1651,[81,81,81,81,81,81,79,79,79,76,76,76,76,54,54,54,54,54,54,54,54,33,33,33,33,20,20,20,0,0],["CC","DD","EE","FF","GG","HH","GG","FF","EE","DD","AA","II","JJ","II","AA","BB","CC","DD","AA"])
getBestFlowRate :: Cave -> Int -> ValveId -> Set ValveId -> State CacheFromToValve (FlowRate, FlowRates, Path)
getBestFlowRate cave minutes initialValveId valvesToOpen = do
    let allAlternatesValvesOrderToOpen = permutations $ Data.List.take 3 $ Set.toList valvesToOpen
    allPathFlowRates <- mapM getBestPathTotalFlowRate allAlternatesValvesOrderToOpen
    return $ maximumBy (compare `on` sel1) $ withStrategy (parBuffer 40 rseq) $ catMaybes allPathFlowRates
    where
        getBestPathTotalFlowRate :: NextValveIds -> State CacheFromToValve (Maybe (FlowRate, FlowRates, Path))
        getBestPathTotalFlowRate = buildBestPathTotalFlowRate [] [initialValveId]

        buildBestPathTotalFlowRate currentFlowRates currentPath [] = do
            let lastValveId = head currentPath
            let lastFlow = sel1 (cave Map.! lastValveId) + headDef 0 currentFlowRates
            let finalFlowRates = replicate (minutes - length currentFlowRates) lastFlow <> currentFlowRates
            return $ Just (sum finalFlowRates, finalFlowRates, currentPath)
        buildBestPathTotalFlowRate currentFlowRates currentPath (nextValveId:nextValveIds) = do 
            maybeTotalFlowRate <- getTotalFlowRateFromPathToValveId cave currentFlowRates currentPath nextValveId
            case maybeTotalFlowRate of
                Nothing -> return Nothing
                Just (newFlowRates, newPath) -> do
                    if length newFlowRates >= minutes
                    then
                        do
                        let finalFlowRates = Data.List.drop (length newFlowRates - minutes) newFlowRates
                        return $ Just (sum finalFlowRates, finalFlowRates, currentPath)
                    else
                        buildBestPathTotalFlowRate newFlowRates newPath nextValveIds
allValvesThatCanBeOpened = Set.fromList . map fst . filter ((0 <) . fst . snd ) . Map.toList 

partA cave = evalState (getBestFlowRate cave 30 "AA" (allValvesThatCanBeOpened cave)) Map.empty

------------ PART B ------------

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
            (totalFlowRatePerson, flowRatesPerson, pathPerson) <- getBestFlowRate cave initialMinutes "AA" valvesToOpenForPerson
            (totalFlowRateElephant, flowRatesElephant, pathElephant) <-  getBestFlowRate cave initialMinutes "AA" valvesToOpenForElephant
            return (totalFlowRateElephant + totalFlowRatePerson, zipWith (+) flowRatesPerson flowRatesElephant , (pathPerson, pathElephant))

maxFlow = maximumBy (compare `on` sel1)
