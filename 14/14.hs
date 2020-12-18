import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data Instruction = SetMask String | SetMem Int Int deriving (Show)

data State = State {mem :: (IntMap Int), mask :: String} deriving (Show)

main = mainB

mainA = do
  contents <- getContents
  print . sum . IntMap.elems . mem . foldl doInstruction (State IntMap.empty "") . map (parseLine) $ lines contents

mainB = do
  contents <- getContents
  print . sum . IntMap.elems . mem . foldl doInstruction2 (State IntMap.empty "") . map (parseLine) $ lines contents

parseLine :: String -> Instruction
parseLine s
  | head parts == "mask" = SetMask value
  | otherwise            = SetMem addr (read value)
  where
    parts = words s
    addr = read . takeWhile (/=']') . tail . dropWhile ( /= '[') $ parts !! 0
    value = parts !! 2

doInstruction :: State -> Instruction -> State
doInstruction (State mem mask) (SetMask x) = State mem x
doInstruction (State mem mask) (SetMem addr value) = 
  let
    maskedValue = applyMask mask value
    updatedMem = IntMap.insert addr maskedValue mem
  in State updatedMem mask

doInstruction2 :: State -> Instruction -> State
doInstruction2 (State mem mask) (SetMask x) = State mem x
doInstruction2 (State mem mask) (SetMem addr value) = 
  let
    addrs = applyAddrMask mask addr
    updatedMem = foldl (\mem addr -> IntMap.insert addr value mem) mem addrs
  in State updatedMem mask

applyMask :: String -> Int -> Int
applyMask s = applyMask' . zip s $ reverse [0..(length s)-1]

applyMask' :: [(Char, Int)] -> Int -> Int
applyMask' [] = id
applyMask' (('0', n):xs) = flip clearBit n . applyMask' xs
applyMask' (('1', n):xs) = flip setBit n . applyMask' xs
applyMask' (_:xs) = applyMask' xs

applyAddrMask :: String -> Int -> [Int]
applyAddrMask s = applyAddrMask' . zip s $ reverse [0..(length s)-1]

applyAddrMask' :: [(Char, Int)] -> Int -> [Int]
applyAddrMask' [] = (:[])
applyAddrMask' (('0', _):xs) = applyAddrMask' xs
applyAddrMask' (('1', n):xs) = map (flip setBit n) . applyAddrMask' xs
applyAddrMask' (('X', n):xs) = concat . map (bothBits n) . applyAddrMask' xs

bothBits :: Int -> Int -> [Int]
bothBits n x = [setBit x n, clearBit x n]
