module Main where

import Control.Monad (foldM, replicateM)
import Control.Monad.Trans.State
import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  input <- readFile filename
  let initState = fromJust $ decodeHex input
  putStrLn "Part 1"
  print $ versionSum $ evalState allPackets initState
  putStrLn "Part 2"
  print $ map evalPacket $ evalState allPackets initState

data Bit = B0 | B1 deriving (Eq, Ord)

instance Show Bit where
  show B0 = "0"
  show B1 = "1"

valueOf :: Bit -> Int
valueOf B0 = 0
valueOf B1 = 1

decodeHex :: String -> Maybe [Bit]
decodeHex =
  foldM
    ( \acc x -> do
        decoded <- decodeHexChar x
        Just (acc ++ decoded)
    )
    []
  where
    decodeHexChar :: Char -> Maybe [Bit]
    decodeHexChar '0' = Just [B0, B0, B0, B0]
    decodeHexChar '1' = Just [B0, B0, B0, B1]
    decodeHexChar '2' = Just [B0, B0, B1, B0]
    decodeHexChar '3' = Just [B0, B0, B1, B1]
    decodeHexChar '4' = Just [B0, B1, B0, B0]
    decodeHexChar '5' = Just [B0, B1, B0, B1]
    decodeHexChar '6' = Just [B0, B1, B1, B0]
    decodeHexChar '7' = Just [B0, B1, B1, B1]
    decodeHexChar '8' = Just [B1, B0, B0, B0]
    decodeHexChar '9' = Just [B1, B0, B0, B1]
    decodeHexChar 'A' = Just [B1, B0, B1, B0]
    decodeHexChar 'B' = Just [B1, B0, B1, B1]
    decodeHexChar 'C' = Just [B1, B1, B0, B0]
    decodeHexChar 'D' = Just [B1, B1, B0, B1]
    decodeHexChar 'E' = Just [B1, B1, B1, B0]
    decodeHexChar 'F' = Just [B1, B1, B1, B1]
    decodeHexChar _ = Nothing

parseInt :: [Bit] -> Int
parseInt bits = sum $ zipWith (\val place -> val * (2 ^ place)) reversedVals [0 ..]
  where
    reversedVals = reverse $ map valueOf bits

parseIntHeader :: Int -> State [Bit] (Maybe Int)
parseIntHeader numBits = do
  bits <- takeBits numBits
  case bits of
    Just x -> (return . Just . parseInt) x
    Nothing -> return Nothing

takeBits :: Int -> State [Bit] (Maybe [Bit])
takeBits numBits = do
  s <- get
  put $ drop numBits s
  let maybeTake :: Int -> [a] -> Maybe [a]
      maybeTake count input
        | length taken == count = Just taken
        | otherwise = Nothing
        where
          taken = take count input
  return $ maybeTake numBits s

data Packet = Packet {pversion :: Int, pdata :: PacketData} deriving (Show)

data PacketData = Literal Int | Operator {subPackets :: [Packet], opType :: OperatorType} deriving (Show)

data OperatorType = SumOp | ProdOp | MinOp | MaxOp | GTOp | LTOp | EqOp deriving (Show)

allPackets :: State [Bit] [Packet]
allPackets = do
  next <- packet
  case next of
    Just pack -> do
      rest <- allPackets
      return $ pack : rest
    Nothing -> return []

packet :: State [Bit] (Maybe Packet)
packet = do
  mversion <- parseIntHeader 3
  mpType <- parseIntHeader 3
  case (mversion, mpType) of
    (Just version, Just pType) -> do
      pd <- packetType pType
      case pd of
        Just pd' -> return $ Just Packet {pversion = version, pdata = pd'}
        Nothing -> return Nothing
    _ -> return Nothing

packetType :: Int -> State [Bit] (Maybe PacketData)
packetType 4 = literal <&> Just
packetType x = do
  len <- get <&> length
  if len > 11
    then operator x <&> Just
    else return Nothing

literal :: State [Bit] PacketData
literal = literalStep <&> (Literal . parseInt)
  where
    literalStep :: State [Bit] [Bit]
    literalStep = do
      next <- takeBits 5
      let Just (x : bits) = next
      case x of
        B0 -> return bits
        B1 -> do
          nextBits <- literalStep
          return $ bits ++ nextBits

operator :: Int -> State [Bit] PacketData
operator op = do
  len <- packetLength
  let opFromInt :: Int -> OperatorType
      opFromInt 0 = SumOp
      opFromInt 1 = ProdOp
      opFromInt 2 = MinOp
      opFromInt 3 = MaxOp
      opFromInt 5 = GTOp
      opFromInt 6 = LTOp
      opFromInt 7 = EqOp
  case len of
    BitLength count -> do
      maybeData <- takeBits count
      let Just subData = maybeData
          subPacks = evalState allPackets subData
      return $ Operator {opType = opFromInt op, subPackets = subPacks}
    SubPackets count -> do
      packets <- replicateM count packet
      let subPacks = map fromJust packets
      return $ Operator {opType = opFromInt op, subPackets = subPacks}

data PacketLength = BitLength Int | SubPackets Int deriving (Show)

packetLength :: State [Bit] PacketLength
packetLength = do
  lengthType <- takeBits 1
  let Just [lengthType'] = lengthType
  case lengthType' of
    B0 -> takeBits 15 <&> (BitLength . parseInt . fromJust)
    B1 -> takeBits 11 <&> (SubPackets . parseInt . fromJust)

versionSum :: [Packet] -> Int
versionSum packets = sum $ concatMap packetVersions packets
  where
    packetVersions :: Packet -> [Int]
    packetVersions Packet {pversion = v, pdata = Literal _} = [v]
    packetVersions Packet {pversion = v, pdata = Operator {subPackets = subPacks}} = v : concatMap packetVersions subPacks

evalPacket :: Packet -> Int
evalPacket Packet {pdata = Literal x} = x
evalPacket Packet {pdata = Operator {opType = SumOp, subPackets = subPacks}} = sum $ map evalPacket subPacks
evalPacket Packet {pdata = Operator {opType = ProdOp, subPackets = subPacks}} = product $ map evalPacket subPacks
evalPacket Packet {pdata = Operator {opType = MinOp, subPackets = subPacks}} = minimum $ map evalPacket subPacks
evalPacket Packet {pdata = Operator {opType = MaxOp, subPackets = subPacks}} = maximum $ map evalPacket subPacks
evalPacket Packet {pdata = Operator {opType = GTOp, subPackets = [x, y]}} = if evalPacket x > evalPacket y then 1 else 0
evalPacket Packet {pdata = Operator {opType = LTOp, subPackets = [x, y]}} = if evalPacket x < evalPacket y then 1 else 0
evalPacket Packet {pdata = Operator {opType = EqOp, subPackets = [x, y]}} = if evalPacket x == evalPacket y then 1 else 0