module Days.Day20 where

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
import GHC.Conc (TVar, newTVar, atomically)
import Data.Scientific (toBoundedInteger)
import Options.Applicative ((<|>), Alternative (empty))
import Control.Monad.ST (runST)
import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import GHC.ST (ST(ST))
import Control.Monad ( zipWithM_, join, forM_ )
import Util.LinkedList

runDay :: R.Day
runDay = R.runDay encryptedFileParser partA partB

------------ PARSER ------------

valueParser :: Parser Value
valueParser = do
    Just value <- toBoundedInteger <$> scientific
    endOfLine <|> endOfInput
    return value

encryptedFileParser :: Parser EncryptedFile
encryptedFileParser = many' valueParser

------------ TYPES ------------

type Value = Int

type EncryptedFile = [Value]
type MixedFile = [Value]

findFirstNextNodeWithValue :: Eq value => value -> Int -> MutableLinkedList value s -> ST s (Maybe (MutableLinkedList value s))
findFirstNextNodeWithValue _ _ Empty = return Nothing
findFirstNextNodeWithValue value stopCounter startNode@(LLNode nodeValue _ nextReference )
    | value == nodeValue = return $ Just startNode
    | otherwise = do
        nextNode <- readSTRef nextReference
        findNext value nextNode (stopCounter - 1)
    where
        findNext :: Eq value => value -> MutableLinkedList value s -> Int ->  ST s (Maybe (MutableLinkedList value s))
        findNext valueToFind _ 0 = return Nothing
        findNext valueToFind Empty _ = return Nothing
        findNext valueToFind nextNode@(LLNode nextNodeValue _ furtherNextReference) nextCounter
            | nextNodeValue == valueToFind = return $ Just nextNode
            | otherwise = do
                furtherNext <- readSTRef furtherNextReference
                findNext valueToFind furtherNext (nextCounter -1)

------------ PART A ------------

-- >>> mixEncryptedFile 1 [1,2,-3,3,-2,0,4]  
-- [0,3,-2,1,2,-3,4]
-- >>> mixEncryptedFile 1 [2,3,0]  
-- [0,3,2]
-- >>> mixEncryptedFile 1 [2,0]  
-- [0,2]
mixEncryptedFile :: Int -> EncryptedFile -> MixedFile
mixEncryptedFile timesToMix encryptedFile = runST $ do
    allMutableNodes <- mapM toMutableNode encryptedFile
    headOfMutableLinkedList <- cyclicLinkNodes allMutableNodes
    forM_ [1..timesToMix] $ \_ -> mapM_ mix allMutableNodes
    Just zeroNode <- findFirstNextNodeWithValue 0 totalSize headOfMutableLinkedList
    toMixedFile zeroNode totalSize
    where
        totalSize = length encryptedFile

        mix :: MutableLinkedList Value s -> ST s (MutableLinkedList Value s)
        mix Empty = return Empty
        mix node@(LLNode value _ _)
            | value `rem` (totalSize - 1) == 0 = return node
            | otherwise = do
                let timesNext = if value > 0 
                                    then
                                        value `rem` (totalSize - 1)
                                    else
                                        (totalSize - 1) + (value `rem` (totalSize - 1))
                nextNode <- next timesNext node
                node `moveAfter` nextNode

        moveAfter Empty _ = return Empty
        moveAfter _ Empty = return Empty
        moveAfter nodeToMove@(LLNode _ toMovePreviousReference toMoveNextReference) afterNode@(LLNode _ _ afterNextReference) = do
            previousNode@(LLNode _ _ previousNodeNextReference) <- readSTRef toMovePreviousReference
            nextNode@(LLNode _ nexNodePreviousReference _) <- readSTRef toMoveNextReference
            nextAfter@(LLNode _ afterNodePreviousReference _) <- readSTRef afterNextReference

            writeSTRef previousNodeNextReference nextNode
            writeSTRef toMoveNextReference nextAfter
            writeSTRef afterNextReference nodeToMove
            writeSTRef afterNodePreviousReference nodeToMove
            writeSTRef nexNodePreviousReference previousNode
            writeSTRef toMovePreviousReference afterNode

            return nodeToMove

        next _ Empty = return Empty
        next 0 node = return node
        next times (LLNode value _ nextReference) = do
            nextNode <- readSTRef nextReference
            next (times - 1) nextNode

        previous _ Empty = return Empty
        previous 0 node = return node
        previous times (LLNode value previousReference _) = do
            previousNode <- readSTRef previousReference
            previous (times -1) previousNode

        cyclicLinkNodes :: [MutableLinkedList Value s] -> ST s (MutableLinkedList Value s)
        cyclicLinkNodes [] = return Empty
        cyclicLinkNodes mutableNodes@(headOfMutableNode:rest) = do
            let shiftedList = rest <> [headOfMutableNode]
            zipWithM_ linkToNext mutableNodes shiftedList
            zipWithM_ linkToPrevious shiftedList mutableNodes
            return headOfMutableNode 

        linkToNext :: MutableLinkedList Value s -> MutableLinkedList Value s -> ST s (MutableLinkedList Value s)
        linkToNext nodeToModify@(LLNode _ _ nextReference) nodeToLinkTo = do
            writeSTRef nextReference nodeToLinkTo
            return nodeToModify

        linkToPrevious :: MutableLinkedList Value s -> MutableLinkedList Value s -> ST s (MutableLinkedList Value s)
        linkToPrevious nodeToModify@(LLNode _ previousReference _) nodeToLinkTo = do
            writeSTRef previousReference nodeToLinkTo
            return nodeToModify

        toMutableNode :: Value -> ST s (MutableLinkedList Value s)
        toMutableNode value = do
            previousNode <- newSTRef Empty
            nextNode <- newSTRef Empty
            return $ LLNode value previousNode nextNode

        toMixedFile :: MutableLinkedList Value s -> Int -> ST s MixedFile
        toMixedFile _ 0 = return []
        toMixedFile Empty _ = return []
        toMixedFile (LLNode value _ nextReference) size = do
            nextNode <- readSTRef nextReference
            (value:) <$> toMixedFile nextNode (size - 1)

getGroveCoordinates :: MixedFile -> Int
getGroveCoordinates mixedFile = 
    sum $ map get [1000, 2000, 3000]
    where
        get = (mixedFile !!) . (`rem` length mixedFile )


-- Part A:
-- 3466
-- (0.20)
partA :: EncryptedFile -> Int
partA = getGroveCoordinates . mixEncryptedFile 1

------------ PART B ------------
-- Part B:
-- 9995532008348
-- (2.22)
partB :: [Value] -> Int
partB = getGroveCoordinates . mixEncryptedFile 10 . map (811589153 *)
