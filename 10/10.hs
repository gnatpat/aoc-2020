import Data.List

main = mainB

mainA = do
  contents <- getContents
  let inValues = sort . map (read) . lines $ contents
  let internal = 3 + last inValues
  let values = inValues ++ [internal]
  let valueDiffs = diffs 0 values
  print $ (countElem valueDiffs 3) * (countElem valueDiffs 1)

mainB = do
  contents <- getContents
  let inValues = sort . map (read) . lines $ contents
  let internal = 3 + last inValues
  let values = inValues ++ [internal]
  print $ countArrangements values (1, 0, 0) 1

diffs :: Int -> [Int] -> [Int]
diffs value [] = []
diffs value (next:rest) = (next - value):(diffs next rest)

countElem :: Eq a => [a] -> a -> Int
countElem [] _ = 0
countElem (x:rest) y
  | x == y    = 1 + countElem rest y
  | otherwise = countElem rest y

countArrangements :: [Int] -> (Int, Int, Int) -> Int -> Int
countArrangements [] (arrangements, _, _) _ = arrangements
countArrangements (adapter:restAdapters) (b1, b2, b3) next
  | adapter /= next = countArrangements (adapter:restAdapters) (0, b1, b2) (next+1)
  | otherwise       = countArrangements restAdapters (b1+b2+b3, b1, b2) (next+1)
