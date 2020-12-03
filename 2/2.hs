import Control.Monad
import Data.Char

main = do
  contents <- getContents
  let count = length . (filter matches2) . lines $ contents
  print count

matches :: String -> Bool
matches input =
  let
    parts = words input
    range = parts !! 0
    char = parts !! 1 !! 0
    seq = parts !! 2
    min_count = read $ takeWhile (/='-') range :: Int
    max_count = read $ tail $ dropWhile (/='-') range :: Int
    count = length $ filter (==char) seq
  in (min_count <= count) && (count <= max_count)

matches2 :: String -> Bool
matches2 input =
  let
    parts = words input
    range = parts !! 0
    char = parts !! 1 !! 0
    seq = parts !! 2
    a = read $ takeWhile (/='-') range :: Int
    b = read $ tail $ dropWhile (/='-') range :: Int
  in (seq !! (a-1) == char) /= (seq !! (b-1) == char)
    

