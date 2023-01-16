module Util.LinkedList where

import Data.STRef (STRef, writeSTRef, readSTRef, newSTRef)
import Control.Monad (join, filterM)
import Control.Monad.ST (ST, runST)
import Control.Monad.Cont (zipWithM_)
import Control.Monad.Identity (Identity)
import GHC.STRef (STRef(STRef))
import Data.Maybe (maybeToList)

type PreviousNode = MutableLinkedList
type NextNode = MutableLinkedList

data MutableLinkedList s value where
    LLNode :: value -> (STRef s (MutableLinkedList s value)) -> (STRef s (MutableLinkedList s value)) -> MutableLinkedList s value
    Empty :: MutableLinkedList s value
        
getValue :: MutableLinkedList s value -> Maybe value
getValue Empty = Nothing
getValue (LLNode value _ _) = Just value

isNodeEmpty :: MutableLinkedList s value -> Bool
isNodeEmpty Empty = True
isNodeEmpty _ = False

isHeadNode :: MutableLinkedList s value -> ST s Bool
isHeadNode Empty = return True
isHeadNode (LLNode _ previousRef _) = do
    previousNode <- readSTRef previousRef
    case previousNode of
        Empty -> return True
        _ -> return False

getHeadNode :: MutableLinkedList s value -> ST s (MutableLinkedList s value)
getHeadNode Empty = return Empty
getHeadNode node@(LLNode _ previousRef _) = do
    previousNode <- readSTRef previousRef
    case previousNode of
        Empty -> return node
        _ -> getHeadNode previousNode

previousNode :: MutableLinkedList s value -> ST s (PreviousNode s value)
previousNode Empty = return Empty
previousNode (LLNode value previousNode _) = readSTRef previousNode

nextNode :: MutableLinkedList s value -> ST s (NextNode s value)
nextNode Empty = return Empty
nextNode (LLNode value _ nextNode) = readSTRef nextNode

previousNthNode :: Int -> MutableLinkedList s value -> ST s (PreviousNode s value)
previousNthNode n node = last . take (n + 1) $ iterate (previousNode =<<) (return node)

nextNthNode :: Int -> MutableLinkedList s value -> ST s (PreviousNode s value)
nextNthNode n node = last . take (n + 1) $ iterate (nextNode =<<) (return node)

-- >>> runST $ getValue <$> (nextNthNode 1 =<< fromList [1,2,3,4])  
-- Just 2
fromList :: [value] -> ST s (MutableLinkedList s value)
fromList [] = return Empty
fromList list = do
    allUnlinkedMutableNodes <- mapM toMutableNode list
    zipWithM_ linkToNext allUnlinkedMutableNodes (tail allUnlinkedMutableNodes)
    zipWithM_ linkToPrevious (reverse allUnlinkedMutableNodes) ((tail . reverse) allUnlinkedMutableNodes)
    return $ head allUnlinkedMutableNodes

linkToNext :: MutableLinkedList s value -> MutableLinkedList s value -> ST s (MutableLinkedList s value)
linkToNext nodeToModify@(LLNode _ _ nextReference) nodeToLinkTo = do
    writeSTRef nextReference nodeToLinkTo
    return nodeToModify

linkToPrevious :: MutableLinkedList s value -> MutableLinkedList s value -> ST s (MutableLinkedList s value)
linkToPrevious nodeToModify@(LLNode _ previousReference _) nodeToLinkTo = do
    writeSTRef previousReference nodeToLinkTo
    return nodeToModify

toMutableNode :: value -> ST s (MutableLinkedList s value)
toMutableNode value = do
    previousNode <- newSTRef Empty
    nextNode <- newSTRef Empty
    return $ LLNode value previousNode nextNode

-- >>> runST $ toList =<< updateNodeValue 9 =<< nextNthNode 3 =<< fromList [1,2,3,4] 
-- [1,2,3,9]
toList :: MutableLinkedList s value -> ST s [value]
toList Empty = return []
toList node = do
    headNode <- getHeadNode node
    toListFromHead headNode
    where
        toListFromHead :: MutableLinkedList s value -> ST s [value]
        toListFromHead Empty = return []
        toListFromHead (LLNode value _ nextRef) = do
            nextNode <- readSTRef nextRef
            nextValues <- toListFromHead nextNode
            return $ [value] <> nextValues

updateNodeValue :: value -> MutableLinkedList s value -> ST s (MutableLinkedList s value)
updateNodeValue newValue nodeWithValueToUpdate = do
    previousRef <- newSTRef Empty
    nextRef <- newSTRef Empty
    let nodeWithNewValue  = LLNode newValue previousRef nextRef
    replaceNode nodeWithValueToUpdate nodeWithNewValue

replaceNode :: MutableLinkedList s value -> MutableLinkedList s value -> ST s (MutableLinkedList s value)
replaceNode Empty nodeToReplaceItWith = return nodeToReplaceItWith
replaceNode nodeToRemove nodeToReplaceItWith@Empty = removeNode nodeToRemove >> return Empty
replaceNode nodeToRemove@(LLNode _ nodeToRemovePreviousRef nodeToRemoveNextRef) nodeToReplaceItWith@(LLNode _ previousRef nextRef) = do
    previousNode <- readSTRef nodeToRemovePreviousRef
    nextNode <- readSTRef nodeToRemoveNextRef
    case previousNode of
        Empty -> return ()
        LLNode _ _ nextRefOfPreviousNode -> writeSTRef nextRefOfPreviousNode nodeToReplaceItWith
    case nextNode of
        Empty -> return ()
        LLNode _ previousRefOfNextNode _ -> writeSTRef previousRefOfNextNode nodeToReplaceItWith
    writeSTRef previousRef previousNode
    writeSTRef nextRef nextNode
    return nodeToReplaceItWith

removeNode :: MutableLinkedList s value -> ST s ()
removeNode Empty = mempty
removeNode (LLNode _ previousRef nextRef) = do
    previousNode <- readSTRef previousRef
    nextNode <- readSTRef nextRef
    case previousNode of
        Empty -> mempty
        LLNode _ _ nextNodeRef -> writeSTRef nextNodeRef nextNode
    case nextNode of
        Empty -> mempty
        LLNode _ previousNodeRef _ -> writeSTRef previousNodeRef nextNode