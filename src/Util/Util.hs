module Util.Util where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Debug.Trace (trace)

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- >>> freq "asdfgaas"
-- fromList [('a',3),('d',1),('f',1),('g',1),('s',2)]

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : (attachCoords x (y + 1) (ls : lss))

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | null ls = []
  | length ls < n = [ls]
  | otherwise = (take n ls) : (chunksOf n (drop n ls))

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
      let (prefix, rest) = span p ls
       in if null prefix
            then (chunksByPredicate p $ dropWhile (not . p) rest)
            else prefix : (chunksByPredicate p $ dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
      | index < 0 -> Nothing
      | index >= (length list) -> Nothing
      | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)

-- >>> takeUntilFirstNRepeats 3 [1,2,3,4,5,6,7,8,9,0,1,2,4,5,1,2,3,4,5,6,7,8,9]
-- [1,2,3,4,5,6,7,8,9,0,1,2,4,5]
takeUntilFirstNRepeats :: Eq a => Int -> [a] -> [a]
takeUntilFirstNRepeats n list = do
  let nRepeat = take n list
  nRepeat ++ takeUntil nRepeat (drop n list)

-- >>> takeUntil [1,2,3] [0,0,0,1,2,1,2,3,4,5,6]
-- [0,0,0,1,2]
-- take until the list is found
takeUntil :: Eq a => [a] -> [a] -> [a]
takeUntil listToCompareTo [] = []
takeUntil listToCompareTo listToSearch@(x : xs)
  | take (length listToCompareTo) listToSearch == listToCompareTo = []
  | otherwise = x : takeUntil listToCompareTo xs

-- >>> aPairAppearsTwice "asddefg"
-- False
-- >>> aPairAppearsTwice "wasddasefg"
-- True
aPairAppearsTwice (a : b : xs) =
  (not . T.null . snd $ T.pack [a, b] `T.breakOn` T.pack xs)
    || aPairAppearsTwice (b : xs)
aPairAppearsTwice _ = False

-- >>> aPairOfTheSameLetterAppearsTwice "aakkaa"
-- True
-- >>> aPairOfTheSameLetterAppearsTwice "aakka"
-- False
-- >>> aPairOfTheSameLetterAppearsTwice "aakkaddkjhyugbjddpgyuh"
-- True
aPairOfTheSameLetterAppearsTwice :: String -> Bool
aPairOfTheSameLetterAppearsTwice (a:b:xs)
    | a /= b = aPairOfTheSameLetterAppearsTwice (b:xs)
    | otherwise = (not . T.null . snd $ T.pack [a, b] `T.breakOn` T.pack xs) || aPairOfTheSameLetterAppearsTwice (b:xs)
aPairOfTheSameLetterAppearsTwice _ = False

-- >>> getFirstRepeating "abcdef"
-- Nothing
-- >>> getFirstRepeating "abcdcef"
-- Just 'c'
-- >>> getFirstRepeating [1,2,3,4,5,2,5,6,1]
-- Just 2
getFirstRepeating :: Ord a => [a] -> Maybe a
getFirstRepeating list = getFirstRepeatingWithAlreadyVisited list Set.empty
  where
    getFirstRepeatingWithAlreadyVisited [] _ = Nothing
    getFirstRepeatingWithAlreadyVisited (x:xs) alreadyVisited
      | x `Set.member` alreadyVisited = Just x
      | otherwise = getFirstRepeatingWithAlreadyVisited xs (Set.insert x alreadyVisited)


-- >>> getNextChar 'a'
-- 'b'
-- >>> getNextChar 'z'
-- 'a'
getNextChar :: Char -> Char
getNextChar 'z' = 'a'
getNextChar c = succ c

-- >>> getNextNthChar 51 'z'
-- 'y'
getNextNthChar :: Int -> Char -> Char
getNextNthChar n c = do
  let nextLetter = fromEnum 'a' + rem (fromEnum c - fromEnum 'a' + n) 26
  toEnum nextLetter

getAllPairs :: Ord b => [b] -> [(b, b)]
getAllPairs list = [(x,y) | x <- list, y <- list, x < y]
