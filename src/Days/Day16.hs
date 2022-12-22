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
import Control.Parallel.Strategies (withStrategy, parBuffer, rseq)
import Data.Either (fromLeft, lefts, rights)


runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

type ValveId = Text

type NextValveIds = [ValveId]

type Path = [ValveId]


type FlowRate = Int
type FlowRates = [FlowRate]

type Valve = (ValveId, (FlowRate, NextValveIds))

type Cave = Map ValveId (FlowRate, NextValveIds)

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
getMaxFlowRatePath :: Cave -> Int -> ValveId -> ((FlowRate, FlowRates, Path, Set ValveId), Cache)
getMaxFlowRatePath cave minutes valveId = runState (getMaxFlowRatePathWithCache cave allValvesThatCanBeOpened (minutes - 1) (0, [0], [valveId], Set.empty) valveId) Map.empty
    where
        allValvesThatCanBeOpened = Set.fromList . map fst . filter ((0 <) . fst . snd ) $ Map.toList cave

type Cache = Map (ValveId, Int, Set ValveId, FlowRate) (FlowRate, FlowRates, Path, Set ValveId)

getMaxFlowRatePathWithCache :: Cave -> Set ValveId -> Int -> (FlowRate, FlowRates, Path, Set ValveId)  -> ValveId -> State Cache (FlowRate, FlowRates, Path, Set ValveId)
getMaxFlowRatePathWithCache cave allValvesThatCanBeOpened minutes flowRatePath@(totalFlowRate, flowRates, path, openedValves) valveId
    | openedValves == allValvesThatCanBeOpened = return (totalFlowRate + lastFlowRate * minutes, replicate minutes lastFlowRate <> flowRates, path, openedValves)
    | minutes <= 0 = return flowRatePath
    | otherwise = do
        inCache <- isInCache
        if inCache 
        then 
            getFromCache
        else do
            bestFlowRatePath <- getBestFlowRatePath
            modify (Map.insert (valveId, minutes, openedValves, totalFlowRate) bestFlowRatePath)
            return bestFlowRatePath

    where
        getBestFlowRatePath :: State Cache (FlowRate, FlowRates, Path, Set ValveId)
        getBestFlowRatePath = do
            if (currentValveFlow > 0) && (valveId `Set.notMember` openedValves)
                then
                    do
                    openedValvePath <- openingValve
                    notOpenedValvePath <- goToNextValves
                    return $ maximumBy (compare `on` sel1) $ withStrategy (parBuffer 40 rseq) [openedValvePath, notOpenedValvePath]
                else
                    goToNextValves

        isInCache :: State Cache Bool
        isInCache = do
            cache <- get
            return $ (valveId, minutes, openedValves, totalFlowRate) `Map.member` cache

        getFromCache :: State Cache (FlowRate, FlowRates, Path, Set ValveId)
        getFromCache = do
            cache <- get
            return $ cache Map.! (valveId, minutes, openedValves, totalFlowRate)

        goToNextValves :: State Cache (FlowRate, FlowRates, Path, Set ValveId)
        goToNextValves = do
            flowRatePaths <- mapM getMaxFlowRateFromNextValves currentNextValves
            return $ maximumBy (compare `on` sel1) flowRatePaths

        getMaxFlowRateFromNextValves :: ValveId -> State Cache (FlowRate, FlowRates, Path, Set ValveId)
        getMaxFlowRateFromNextValves nextValveId = do
            let nextMinutes = minutes - 1
            let newFlowRatePath = (totalFlowRate + lastFlowRate, lastFlowRate:flowRates, nextValveId:path, openedValves)
            getMaxFlowRatePathWithCache cave allValvesThatCanBeOpened nextMinutes newFlowRatePath nextValveId

        openingValve :: State Cache (FlowRate, FlowRates, Path, Set ValveId)
        openingValve = do
            let nextMinutes = minutes - 1
            let newOpenedVaves = Set.insert valveId openedValves
            let newFlowRatePath = (totalFlowRate + lastFlowRate + currentValveFlow, (lastFlowRate + currentValveFlow):flowRates, path, newOpenedVaves)
            getMaxFlowRatePathWithCache cave allValvesThatCanBeOpened nextMinutes newFlowRatePath valveId

        lastFlowRate = headDef 0 flowRates
        
        currentValve = cave Map.! valveId 
        currentValveFlow = fst currentValve
        currentNextValves = snd currentValve

partA :: Cave -> (FlowRate, FlowRates, Path, Set ValveId)
-- partA cave = sel1 $ getMaxFlowRatePath cave 30 "AA"
partA cave = error "Not implemented yet!"

------------ PART B ------------

type CachePartB = Map (Set ValveId, Int, Set ValveId, FlowRate) TotalFlowRateState

type OpenedValves = Set ValveId

type TotalFlowRate = Int

type TotalFlowRateState = (TotalFlowRate, FlowRates, (Path, Path), OpenedValves)

type OpenCloseValveId = Either ValveId ValveId 

getMaxFlowRatePath2Persons :: Cave -> Int -> [ValveId] -> (TotalFlowRateState, CachePartB)
getMaxFlowRatePath2Persons cave minutes valveIds = runState (getMaxFlowRatePath2PersonsWithCache cave allValvesThatCanBeOpened (minutes - 1) (0, [0], (valveIds, valveIds), Set.empty)) Map.empty
    where
        allValvesThatCanBeOpened = Set.fromList . map fst . filter ((0 <) . fst . snd ) $ Map.toList cave

getMaxFlowRatePath2PersonsWithCache :: Cave -> Set ValveId -> Int -> TotalFlowRateState -> State CachePartB TotalFlowRateState
getMaxFlowRatePath2PersonsWithCache cave allValvesThatCanBeOpened minutes flowRatePath@(totalFlowRate, flowRates, paths@(leftPath, rightPath), openedValves)
    | openedValves == allValvesThatCanBeOpened = return (totalFlowRate + lastFlowRate * minutes, replicate minutes lastFlowRate <> flowRates, paths, openedValves)
    | minutes <= 0 = return flowRatePath
    | otherwise = do
        inCache <- isInCache
        if inCache 
        then 
            getFromCache
        else do
            bestFlowRatePath <- getBestFlowRatePath
            modify (Map.insert (currentValveIdsSet, minutes, openedValves, totalFlowRate) bestFlowRatePath)
            return bestFlowRatePath

    where
        getBestFlowRatePath :: State CachePartB TotalFlowRateState
        getBestFlowRatePath = do
            leftOpenRightClose <- openValvesOrGoToNext (open leftValveId, close rightValveId)
            leftOpenRightOpen <- openValvesOrGoToNext (open leftValveId, open rightValveId)
            leftCloseRightOpen <- openValvesOrGoToNext (close leftValveId, open rightValveId)
            leftCloseRightClose <- openValvesOrGoToNext (close leftValveId, close rightValveId)

            return $ maxFlow $ withStrategy (parBuffer 40 rseq) $ leftOpenRightClose <> leftOpenRightOpen <> leftCloseRightOpen <> leftCloseRightClose

        openValvesOrGoToNext :: (OpenCloseValveId, OpenCloseValveId) -> State CachePartB [TotalFlowRateState]
        openValvesOrGoToNext (eitherLeftValveId, eitherRightValveId) = do
               if isNotAlreadyOpened eitherLeftValveId && isNotAlreadyOpened eitherRightValveId
                        then do
                            let leftValveIdToOpen = lefts [eitherLeftValveId] 
                            let rightValveIdToOpen = lefts [eitherRightValveId] 
                            let openingValveIds = Set.fromList $ leftValveIdToOpen <> rightValveIdToOpen
                            let newOpenedVaves = Set.union openedValves openingValveIds
                            let currentValveFlows = sum . map getValveFlow $ Set.toList openingValveIds
                            let nextLeftValveIds =  getNextValves $ rights [eitherLeftValveId]
                            let nextRightValveIds = getNextValves $ rights [eitherRightValveId]
                            let newFlowRatePath (nextLeftValveId, nextRightValveId) = (totalFlowRate + lastFlowRate + currentValveFlows, (lastFlowRate + currentValveFlows):flowRates, (nextLeftValveId <> leftPath, nextRightValveId <> rightPath), newOpenedVaves)
                            let allNextValveIdsPairs = Set.toList $ Set.cartesianProduct (Set.fromList nextLeftValveIds) (Set.fromList nextRightValveIds)
                            let nextMinutes = minutes - 1
                            mapM (getMaxFlowRatePath2PersonsWithCache cave allValvesThatCanBeOpened nextMinutes . newFlowRatePath) allNextValveIdsPairs
                        else return []

        isNotAlreadyOpened :: OpenCloseValveId -> Bool
        isNotAlreadyOpened = either (`Set.notMember` openedValves) (const True)

        isInCache :: State CachePartB Bool
        isInCache = do
            cache <- get
            return $ (currentValveIdsSet, minutes, openedValves, totalFlowRate) `Map.member` cache

        getFromCache :: State CachePartB TotalFlowRateState
        getFromCache = do
            cache <- get
            return $ cache Map.! (currentValveIdsSet, minutes, openedValves, totalFlowRate)

        leftValveId = head leftPath
        leftValveFlow = getValveFlow leftValveId

        open = Left
        close = Right

        rightValveId = head rightPath
        rightValveFlow = getValveFlow rightValveId

        currentValveIdsSet = Set.fromList [leftValveId, rightValveId]

        lastFlowRate = headDef 0 flowRates
        
        getValve = (Map.!) cave 
        getValveFlow = fst . getValve


        getNextValves :: [ValveId] -> [[ValveId]]
        getNextValves [] = [[]]
        getNextValves valveIds = map (:[]) $ concatMap (snd . getValve) valveIds

        maxFlow :: [TotalFlowRateState] -> TotalFlowRateState
        maxFlow = maximumBy (compare `on` sel1)

partB :: Cave -> TotalFlowRateState
partB cave = sel1 $ getMaxFlowRatePath2Persons cave 26 ["AA"]
