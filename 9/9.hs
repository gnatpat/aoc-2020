main = mainB

mainA = do
  contents <- getContents
  let values = map read . lines $ contents
  print . uncurry findNotSum $ splitAt 25 values

mainB = do
  contents <- getContents
  let values = map read . lines $ contents
  let value = uncurry findNotSum $ splitAt 25 values
  let contiguous = findContiguousSum values value
  print ((minimum contiguous) + (maximum contiguous))
  

findNotSum :: [Int] -> [Int] -> Int
findNotSum prev (current:rest)
 | any (\x -> (current - x) `elem` prev) . filter (\x -> x `div` 2 /= current) $ prev = findNotSum (current:(take 24 prev)) rest
 | otherwise = current

findContiguousSum :: [Int] -> Int -> [Int]
findContiguousSum values value = findContiguousSum' [head values] (tail values) value

findContiguousSum' :: [Int] -> [Int] -> Int -> [Int]
findContiguousSum' values rest value
  | sumValues == value = values
  | sumValues > value  = findContiguousSum' (tail values) rest value
  | otherwise          = findContiguousSum' (values ++ [head rest]) (tail rest) value
  where
    sumValues = sum values
