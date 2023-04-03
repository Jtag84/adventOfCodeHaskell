module Year2019.Day23 (runDay) where

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
import Year2019.IntCodeComputer (opcodesParser, startProgramState, setInputs, ProgramState, getOutputs, executeOneStepIntCodeProgram, clearOutputs, Outputs, appendToInputs, Opcode, isProgramNeedInput, getInputs, isProgramRunning, clearIntCodeProgram)
import Util.Util (UnShow(UnShow))
import Data.Tuple.All (Sel1(sel1), Sel3 (sel3), Sel2 (sel2))

runDay :: R.Day
runDay = R.runDay opcodesParser partA partB

------------ TYPES ------------
type Address = Int
type X = Int
type Y = Int 
type NetworkCards = Map.Map Address ProgramState

type Packet = (X,Y)
type Packets = Map.Map Address [Packet]

type SentNatPacket = [Packet]
type IdleState = Map.Map Address Bool
type NatPacket = (Packet, SentNatPacket, IdleState)
------------ PART A ------------

initNic :: [Opcode] -> NetworkCards
initNic opcodes = Map.fromList . map (\address -> (address, setInputs [address] (startProgramState opcodes))) $ [0..49]

executeOneStepOfNetwork :: (Maybe NatPacket, Packets, NetworkCards) -> (Maybe NatPacket, NetworkCards)
executeOneStepOfNetwork (natPacket, allNewPacketsByAddress, networkCardStates) = do
    let clearedPacketOutputsStates = Map.map (clearPacketOutputs . rerunWithDefaultInputIfNeedsInput) networkCardStates
    let networkCardsWithNewPacketInputs = Map.mapWithKey (routePacketsToInputs allNewPacketsByAddress) clearedPacketOutputsStates
    (natPacket, Map.map executeOneStepIntCodeProgram networkCardsWithNewPacketInputs)
    where
        clearPacketOutputs programState = 
            if (length . getOutputs) programState >= 3 
                then clearOutputs programState 
                else programState
        routePacketsToInputs packets address programState = do
            let packetToAppend = maybe [] (concatMap (\(x,y) -> [x,y])) $ packets Map.!? address
            appendToInputs packetToAppend programState
        rerunWithDefaultInputIfNeedsInput programState =
                    if isProgramNeedInput programState
                        then executeOneStepIntCodeProgram $ appendToInputs [-1] programState
                        else programState
            
toPacket (address:x:y:[]) = (address, [(x,y)])

getPackets = Map.fromListWith (++) . map toPacket . filter ((==3) . length) . map getOutputs . Map.elems

isPacketSentToAddress255 = isJust . fst

getNatPacketWithPackets :: (Maybe NatPacket, NetworkCards) -> (Maybe NatPacket, Packets, NetworkCards)
getNatPacketWithPackets (natPacket, networkCardsStates) = do
    let packets = getPackets networkCardsStates
    let newNatPacket = 
            if isJust $ packets Map.!? 255 
                then Just (head $ packets Map.! 255, maybe [] sel2 natPacket, maybe Map.empty sel3 natPacket)
                else natPacket
    (newNatPacket, packets, networkCardsStates)

-- Part A:
-- Just 17286
-- (0.293744s)
partA opcodes = snd . sel1 <$> (fst . until isPacketSentToAddress255 (executeOneStepOfNetwork . getNatPacketWithPackets) $ (Nothing, initNic opcodes))

------------ PART B ------------

processNat :: (Maybe NatPacket, Packets, NetworkCards) -> (Maybe NatPacket, Packets, NetworkCards)
processNat state@(Nothing, _, _) = state
processNat (Just (currentNatPacket, natPacketsSent, idleStates), packets, networkCardsState) = do
    let newIdleState = Map.mapWithKey isIdle networkCardsState
    let allIdle = all (==True) newIdleState
    if allIdle
        then (Just(currentNatPacket, currentNatPacket:natPacketsSent, newIdleState), Map.singleton 0 [currentNatPacket], networkCardsState)
        else (Just (currentNatPacket, natPacketsSent, newIdleState), packets, networkCardsState)
    where
        isIdle address nic = do
            let noOutputs = null . getOutputs  $ nic
            let noPackets = isNothing $ packets Map.!? address
            let noInputs = null . getInputs $ nic
            let wasAlreadyIdle = Map.findWithDefault False address idleStates
            let isNeedInput = isProgramNeedInput nic
            noOutputs && noPackets && noInputs && (isNeedInput || wasAlreadyIdle)

sameYValueSentTwiceByNat :: (Maybe NatPacket, NetworkCards)  -> Bool
sameYValueSentTwiceByNat (Nothing, _) = False
sameYValueSentTwiceByNat (Just (_, natPacketSent, _), _) = not . null . getYValuesSentTwiceByNat $ natPacketSent

getYValuesSentTwiceByNat :: [Packet] -> [[Y]]
getYValuesSentTwiceByNat = filter ((>=2) . length) . group . sort . map snd 

-- Part B:
-- Just 11249
-- (17.214742s)
partB opcodes = snd . sel1 <$> (fst . until sameYValueSentTwiceByNat (executeOneStepOfNetwork . processNat . getNatPacketWithPackets) $ (Nothing, initNic opcodes))