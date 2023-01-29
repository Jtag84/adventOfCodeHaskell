module Year2018.Day08 (runDay) where

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
import Safe (atDef, atMay)

runDay :: R.Day
runDay = R.runDay nodeParser partA partB

------------ TYPES ------------ 
type Metadata = Int
data Node = Node [Node] [Metadata]
    deriving(Eq, Show)

------------ PARSER ------------

nodeParser :: Parser Node
nodeParser = do
    Just numberOfNodes <- toBoundedInteger <$> scientific
    " "
    Just numberOfMetadata <- toBoundedInteger <$> scientific
    " "
    nodes <- count numberOfNodes nodeParser
    allMetadata <- count numberOfMetadata $ fromJust . toBoundedInteger <$> scientific <* P.choice [void " ", endOfLine, endOfInput]
    return $ Node nodes allMetadata

------------ PART A ------------
getAllMetadata :: Node -> [Metadata] 
getAllMetadata (Node [] metadata) = metadata
getAllMetadata (Node nodes metadata) = concatMap getAllMetadata nodes <> metadata

-- Part A:
-- 46096
-- (0.001625s)
partA = sum . getAllMetadata

------------ PART B ------------

getNodeValue :: Node -> Int
getNodeValue (Node [] metadata) = sum metadata
getNodeValue (Node nodes metadata) = sum . map (getNodeValue . fromJust) . filter isJust . map (atMay nodes . (+ (-1))) $ metadata

-- Part B:
-- 24820
-- (0.001592s)
partB = getNodeValue