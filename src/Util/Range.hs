module Util.Range where

data Range where
  EmptyRange :: Range
  Range :: (Int, Int) -> Maybe Range-> Range
  deriving(Show, Eq, Ord)

-- >>> isInRange 3 (Range (1,5) Nothing)
-- True
-- >>> isInRange 3 (Range (4,5) Nothing)
-- False
-- >>> isInRange 3 (Range (4,5) (Just (Range (1,5) Nothing)))
-- False
-- >>> isInRange 3 (Range (1,2) (Just (Range (4,5) Nothing)))
-- False
-- >>> isInRange 8 (Range (1,2) (Just (Range (4,5) (Just (Range (8,9) Nothing )))))
-- True
isInRange :: Int -> Range -> Bool
isInRange value EmptyRange = False
isInRange value (Range (start, end) maybeNextRange)
  | value < start = False
  | otherwise = value >= start && value <= end || isInMaybeRange
  where
    isInMaybeRange = 
      case maybeNextRange of
        Nothing -> False
        Just nextRange -> isInRange value nextRange

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  (<>) EmptyRange EmptyRange = EmptyRange
  (<>) left EmptyRange = left
  (<>) EmptyRange right = right
  (<>) (Range left@(startLeft, endLeft) maybeNextLeft) (Range right@(startRight, endRight) maybeNextRight)
    | overlaps left right || contiguous left right = Range (min startLeft startRight, max endLeft endRight) Nothing <> joinMaybeRanges maybeNextLeft maybeNextRight
    | otherwise = Range (min left right) $ Just $ Range (max left right) Nothing <> joinMaybeRanges maybeNextLeft maybeNextRight
    where
      joinMaybeRanges (Just left) (Just right) = left <> right
      joinMaybeRanges (Just left) Nothing = left
      joinMaybeRanges Nothing (Just right) = right
      joinMaybeRanges Nothing Nothing = EmptyRange

-- >>> Range (4,5) Nothing <> EmptyRange
-- Range (4,5) Nothing
-- >>> Range (4,5) Nothing <> Range (1,3) Nothing
-- Range (1,3) (Just (Range (4,5) Nothing))
-- >>> Range (4,6) Nothing <> Range (1,5) Nothing
-- Range (1,6) Nothing
-- >>> Range (1,3) (Just (Range (4,6) Nothing)) <> Range (3,4) Nothing
-- Range (1,6) Nothing
-- >>> Range (1,3) (Just (Range (4,6) Nothing)) <> Range (3,4) (Just (Range (9,16) Nothing))
-- Range (1,6) (Just (Range (9,16) Nothing))
-- >>> Range (1,3) (Just (Range (9,16) Nothing)) <> Range (3,4) (Just (Range (5,6) Nothing))
-- Range (1,4) (Just (Range (5,6) (Just (Range (9,16) Nothing))))
-- >>> Range (1,6) Nothing <> Range (9,16) Nothing
-- Range (1,6) (Just (Range (9,16) Nothing))
instance Monoid Range where
  mempty :: Range
  mempty = EmptyRange

-- >>> countRange $ Range (9,16) Nothing
-- 8
-- >>> countRange $ Range (1,6) Nothing
-- 6
-- >>> countRange $ Range (1,6) (Just (Range (9,16) Nothing))
-- 14
countRange :: Range -> Int
countRange EmptyRange = 0
countRange (Range (start, end) maybeNextRange)  = end - start + 1 + countMaybeRange
  where 
    countMaybeRange = maybe 0 countRange maybeNextRange
  

contiguous :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
contiguous (startLeft, endLeft) (startRight, endRight) = (endLeft + 1) == startRight || (endRight + 1) == startLeft

-- >>> overlaps (1,4) (3, 9)
-- >>> overlaps (4,1) (3, 9)
-- >>> overlaps (3,1) (3, 9)
-- >>> overlaps (9,10) (3, 9)
-- True
-- True
-- True
-- True
-- >>> overlaps (10,14) (3, 9)
-- False
-- >>> (-880021,1602225) `overlaps` (281841,1076513)
overlaps :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
overlaps (startLeft, endLeft) right@(startRight, endRight) = 
    isBetween startLeft right 
    || isBetween endLeft right 
    || startLeft <= startRight &&  endLeft >= endRight 
    || startRight <= startLeft &&  endRight >= endLeft 
  where
    isBetween point (start, end) = ((end - point) * (point - start)) >= 0

-- >>> mergeRanges [Range (-880021,1076513) Nothing,Range (1076513,1602225) Nothing,Range (281841,1076513) Nothing,EmptyRange,Range (2723995,3796335) Nothing,EmptyRange,Range (3612589,3754037) Nothing,Range (1036311,1076513) Nothing,EmptyRange,EmptyRange]
-- Range (-880021,1602225) (Just (Range (2723995,3796335) Nothing))
mergeRanges :: [Range] -> Range
mergeRanges = foldl (<>) EmptyRange

inverseWithinRange :: Range -> [Range]
inverseWithinRange EmptyRange = [] 
inverseWithinRange (Range (start, end) Nothing) = [] 
inverseWithinRange (Range (start, end) (Just EmptyRange) ) = [] 
inverseWithinRange (Range (start, end) (Just  next@(Range (nextStart, _) maybeNextNext))) = Range (end + 1, nextStart - 1) Nothing:inverseWithinRange next
