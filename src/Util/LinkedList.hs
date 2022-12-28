module Util.LinkedList where

import Data.STRef (STRef)

type PreviousNode = LinkedListNode
type NextNode = LinkedListNode

data LinkedListNode value wrapper where
    LLNode :: value -> (wrapper (PreviousNode value wrapper)) -> (wrapper (NextNode value wrapper)) -> LinkedListNode value wrapper
    Empty :: LinkedListNode value wrapper
        
type MutableLinkedList a s = LinkedListNode a (STRef s)