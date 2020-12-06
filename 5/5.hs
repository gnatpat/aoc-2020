import Control.Monad
import Data.Char
import Data.List

main = mainB

mainA = do
  contents <- getContents
  print . maximum . map getSeatId . lines $ contents

mainB = do
  contents <- getContents
  print . findFirstGap . sort . map getSeatId . lines $ contents

getSeatId :: String -> Int
getSeatId seat = 
    let rowString = take 7 seat
        columnString = drop 7 seat
    in (getRow rowString) * 8 + (getColumn columnString)

getRow :: String -> Int
getRow = bsp 'F' 'B' 0 64

getColumn :: String -> Int
getColumn = bsp 'L' 'R' 0 4

bsp :: Char -> Char -> Int -> Int -> String -> Int
bsp _ _ value _ [] = value
bsp lower upper value addition (x:xs)
  | x == lower = bsp lower upper value nextAddition xs
  | x == upper = bsp lower upper (value + addition) nextAddition xs
  where
    nextAddition = addition `div` 2

findFirstGap :: [Int] -> Int
findFirstGap (x:y:xs)
  | x + 1 == y = findFirstGap (y:xs)
  | otherwise  = x+1
