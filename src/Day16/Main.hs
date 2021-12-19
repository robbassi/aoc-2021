module Main where

import Data.Bits
import Data.Functor ((<&>))

type Version = Int
type TypeId = Int

data LengthType = Bits Int | Packets Int
  deriving Show

data Packet
  = Sum Version [Packet]
  | Product Version [Packet]
  | Min Version [Packet]
  | Max Version [Packet]
  | GreaterThan Version Packet Packet
  | LessThan Version Packet Packet
  | EqualTo Version Packet Packet
  | Literal Version Int
  deriving Show

data Bin = One | Zero
  deriving (Show, Eq)

hexToBin :: Char -> [Bin]
hexToBin '0' = [Zero,Zero,Zero,Zero]
hexToBin '1' = [Zero,Zero,Zero,One]
hexToBin '2' = [Zero,Zero,One,Zero]
hexToBin '3' = [Zero,Zero,One,One]
hexToBin '4' = [Zero,One,Zero,Zero]
hexToBin '5' = [Zero,One,Zero,One]
hexToBin '6' = [Zero,One,One,Zero]
hexToBin '7' = [Zero,One,One,One]
hexToBin '8' = [One,Zero,Zero,Zero]
hexToBin '9' = [One,Zero,Zero,One]
hexToBin 'A' = [One,Zero,One,Zero]
hexToBin 'B' = [One,Zero,One,One]
hexToBin 'C' = [One,One,Zero,Zero]
hexToBin 'D' = [One,One,Zero,One]
hexToBin 'E' = [One,One,One,Zero]
hexToBin 'F' = [One,One,One,One]
hexToBin _ = []

binToDec :: [Bin] -> Int
binToDec b = encode b 0
  where
    encode [] n = n
    encode (Zero:rest) n = encode rest (shift n 1)
    encode (One:rest) n = encode rest (shift n 1 + 1)

sumVersions :: Packet -> Int
sumVersions = go 0
  where
    go n (Literal v _) = v + n
    go n (Sum v c) = v + n + sum (c <&> sumVersions)
    go n (Product v c) = v + n + sum (c <&> sumVersions)
    go n (Min v c) = v + n + sum (c <&> sumVersions)
    go n (Max v c) = v + n + sum (c <&> sumVersions)
    go n (LessThan v a b) = v + n + sumVersions a + sumVersions b
    go n (GreaterThan v a b) = v + n + sumVersions a + sumVersions b
    go n (EqualTo v a b) = v + n + sumVersions a + sumVersions b

eval :: Packet -> Int
eval (Literal _ n) = n
eval (Sum _ c) = sum (c <&> eval)
eval (Product _ c) = product (c <&> eval)
eval (Min _ c) = minimum (c <&> eval)
eval (Max _ c) = maximum (c <&> eval)
eval (GreaterThan _ a b)
  | eval a > eval b = 1
  | otherwise = 0
eval (LessThan _ a b)
  | eval a < eval b = 1
  | otherwise = 0
eval (EqualTo _ a b)
  | eval a == eval b = 1
  | otherwise = 0

readPacket :: String -> Packet
readPacket hex = fst $ parsePacket $ hex >>= hexToBin
  where
    parsePacket bits =
      let (version, typeId, bits') = parseHeader bits
       in parsePacketBody version typeId bits'
    parseHeader (a:b:c:d:e:f:rest) =
      let version = binToDec [a,b,c]
          typeId = binToDec [d,e,f]
       in (version, typeId, rest)
    parsePacketBody version 4 bits =
      let parseLiteral (Zero:a:b:c:d:rest) n =
            (shift n 4 .|. binToDec [a,b,c,d], rest)
          parseLiteral (One:a:b:c:d:rest) n =
            parseLiteral rest (shift n 4 .|. binToDec [a,b,c,d])
          (num, rest) = parseLiteral bits 0
       in (Literal version num, rest)
    parsePacketBody version typeId (Zero:bits) =
      let len = fromIntegral $ binToDec $ take 15 bits
          bits' = drop 15 bits
          parseChildren [] children = reverse children
          parseChildren bits children =
            let (packet, bits') = parsePacket bits
             in parseChildren bits' (packet:children)
          children = parseChildren (take len bits') []
       in (buildOperator typeId version children, drop len bits')
    parsePacketBody version typeId (One:bits) =
      let len = fromIntegral $ binToDec $ take 11 bits
          bits' = drop 11 bits
          parseChildren 0 bits children = (reverse children, bits)
          parseChildren packets bits children =
            let (packet, bits') = parsePacket bits
             in parseChildren (pred packets) bits' (packet:children)
          (children, bits'') = parseChildren len bits' []
       in (buildOperator typeId version children, bits'')
    buildOperator 0 = Sum
    buildOperator 1 = Product
    buildOperator 2 = Min
    buildOperator 3 = Max
    buildOperator 5 = \v [a,b] -> GreaterThan v a b
    buildOperator 6 = \v [a,b] -> LessThan v a b
    buildOperator 7 = \v [a,b] -> EqualTo v a b

main :: IO ()
main = do
  input <- getContents
  let packet = readPacket input
  print $ "part 1 = " ++ show (sumVersions packet)
  print $ "part 2 = " ++ show (eval packet)