module Days.Day13 (runDay) where

import Data.Attoparsec.Text (Parser, char, endOfLine, many', scientific, sepBy')
import Data.List (elemIndex, sort)
import Data.Scientific (Scientific)
import Options.Applicative ((<|>))
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
type PacketPair = (Packet, Packet)

type Packet = [PacketElement]

data PacketElement where
  List :: [PacketElement] -> PacketElement
  Value :: Scientific -> PacketElement
  deriving (Show, Eq)

instance Ord PacketElement where
  compare :: PacketElement -> PacketElement -> Ordering
  compare (Value left) (Value right) = compare left right
  compare (List []) (List []) = EQ
  compare (List []) (List (x : _)) = LT
  compare (List (x : _)) (List []) = GT
  compare left@(Value _) right@(List _) = compare (List [left]) right
  compare left@(List _) right@(Value _) = compare left (List [right])
  compare (List (xLeft : xsLeft)) (List (xRight : xsRight)) = do
    let firstElementsComparison = compare xLeft xRight
    case firstElementsComparison of
      EQ -> compare (List xsLeft) (List xsRight)
      _ -> firstElementsComparison

------------ PARSER ------------
inputParser :: Parser [PacketPair]
inputParser = many' packetPairParser

packetPairParser :: Parser PacketPair
packetPairParser = do
  left <- packetParser
  right <- packetParser
  endOfLine
  return (left, right)

packetParser :: Parser Packet
packetParser = many' packetElementListParser <* endOfLine

packetElementValueParser :: Parser PacketElement
packetElementValueParser = Value <$> scientific

packetElementListParser :: Parser PacketElement
packetElementListParser = do
  char '['
  packetElement <- (packetElementValueParser <|> packetElementListParser) `sepBy'` char ','
  char ']'
  return $ List packetElement

------------ PART A ------------
partA :: (Num sumOfOrderedPairs, Enum sumOfOrderedPairs) => [PacketPair] -> sumOfOrderedPairs
partA = sum . map fst . filter snd . zip [1 ..] . map (uncurry (<))

------------ PART B ------------
partB :: [PacketPair] -> Int
partB inputs = do
  let dividerPacket2 = [List [Value 2]]
  let dividerPacket6 = [List [Value 6]]
  let packets = concatMap (\packetPair -> [fst packetPair, snd packetPair]) inputs
  let packetsWithDividers = sort (dividerPacket2 : dividerPacket6 : packets)
  let Just divider2packetIndex = elemIndex dividerPacket2 packetsWithDividers
  let Just divider6packetIndex = elemIndex dividerPacket6 packetsWithDividers
  (divider2packetIndex + 1) * (divider6packetIndex + 1)