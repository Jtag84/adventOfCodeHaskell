module Util.LinkedList where

import Data.STRef (STRef, writeSTRef, readSTRef, newSTRef)
import Control.Monad (join, filterM)
import Control.Monad.ST (ST, runST)
import Control.Monad.Cont (zipWithM_)
import Control.Monad.Identity (Identity)

type PreviousNode = LinkedListNode
type NextNode = LinkedListNode

data LinkedListNode value m where
    LLNode :: value -> (m (PreviousNode value m)) -> (m (NextNode value m)) -> LinkedListNode value m
    Empty :: LinkedListNode value m
        
type MutableLinkedList a s = LinkedListNode a (STRef s)

getValue :: LinkedListNode value m -> Maybe value
getValue Empty = Nothing
getValue (LLNode value _ _) = Just value

-- isMutableHeadNode :: LinkedListNode value m -> m Bool
-- isMutableHeadNode Empty = return True
-- isMutableHeadNode (LLNode _ previousRef _) = do
--     previousNode <- readSTRef previousRef
--     case previousNode of
--         Empty -> return True
--         _ -> return False


-- getMutableHeadNode :: Monad m => LinkedListNode value m -> (PreviousNode value m)
-- getMutableHeadNode Empty = Empty
-- getMutableHeadNode node = last . take 2 . filterM (isMutableHeadNode) $ iterate (previousNode =<<) (return node)

previousNode :: Monad m => LinkedListNode value m -> m (PreviousNode value m)
previousNode Empty = return Empty
previousNode (LLNode value previousNode _) = previousNode

nextNode :: Monad m => LinkedListNode value m -> m (NextNode value m)
nextNode Empty = return Empty
nextNode (LLNode value _ nextNode) = nextNode

previousNthNode :: Monad m => Int -> LinkedListNode value m -> m (PreviousNode value m)
previousNthNode n node = last . take n $ iterate (previousNode =<<) (return node)

nextNthNode :: Monad m => Int -> LinkedListNode value m -> m (PreviousNode value m)
nextNthNode n node = last . take n $ iterate (previousNode =<<) (return node)

-- >>> runST $  nextNode <$>  fromList [1,2,3,4]  
-- Just 1
fromList :: [value] -> ST s (MutableLinkedList value s)
fromList [] = return Empty
fromList list = do
    allUnlinkedMutableNodes <- mapM toMutableNode list
    zipWithM_ linkToNext allUnlinkedMutableNodes (tail allUnlinkedMutableNodes)
    zipWithM_ linkToPrevious (reverse allUnlinkedMutableNodes) ((tail . reverse) allUnlinkedMutableNodes)
    return $ head allUnlinkedMutableNodes

linkToNext :: MutableLinkedList value s -> MutableLinkedList value s -> ST s (MutableLinkedList value s)
linkToNext nodeToModify@(LLNode _ _ nextReference) nodeToLinkTo = do
    writeSTRef nextReference nodeToLinkTo
    return nodeToModify

linkToPrevious :: MutableLinkedList value s -> MutableLinkedList value s -> ST s (MutableLinkedList value s)
linkToPrevious nodeToModify@(LLNode _ previousReference _) nodeToLinkTo = do
    writeSTRef previousReference nodeToLinkTo
    return nodeToModify

toMutableNode :: value -> ST s (MutableLinkedList value s)
toMutableNode value = do
    previousNode <- newSTRef Empty
    nextNode <- newSTRef Empty
    return $ LLNode value previousNode nextNode

-- toImmutableLinkedList :: MutableLinkedList value s -> ST s (LinkedListNode value Identity)
-- toImmutableLinkedList Empty = return Empty
-- toImmutableLinkedList node@(LLNode value previousRef nextRef) = do
--     let headNode = getHeadNode node
--     return node
--     where
--         toImmutableWithPreviousAndNext value Empty nextNode = LLNode value Empty (toImmutableLinkedList nextNode)
--         toImmutableWithPreviousAndNext value previousNode Empty = LLNode value Empty (toImmutableLinkedList nextNode)




-- updateNodeValue :: value -> MutableLinkedList value s -> MutableLinkedList value s
-- updateNodeValue newValue Empty = LLNode value Empty Empty
-- updateNodeValue newValue (LLNode currentValue (LLNode _ _ previousNextNode) (LLNode _ nextPreviousNode _)) = do
--     let nodeWithNewValue  = LLNode newValue previousNode nextNode
--     LLNode _ _ previousNextNode <- previousNode

--     writeSTRef (STRef s a) a

replaceNode :: MutableLinkedList value s -> MutableLinkedList value s -> ST s (MutableLinkedList value s)
replaceNode Empty nodeToReplaceItWith = return nodeToReplaceItWith
replaceNode nodeToRemove nodeToReplaceItWith@Empty = removeNode nodeToRemove >> return Empty

removeNode :: MutableLinkedList value s -> ST s ()
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
        

