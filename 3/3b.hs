import Control.Monad
import Data.Char

main = do
  contents <- getContents
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      count = foldr1 (*) . foldr1 (zipWith (+)) . map (areTrees slopes) . (zip [0..]) . lines $ contents
  print count

areTrees :: [(Int, Int)] -> (Int, String) -> [Int]
areTrees slopes (step, s) = map (fromEnum . isTree step s) slopes

isTree :: Int -> String -> (Int, Int) -> Bool
isTree step s (x, y)
  | mod step y /= 0 = False
  | otherwise       = (s !! mod (x * (step `div` y)) (length s)) == '#'
