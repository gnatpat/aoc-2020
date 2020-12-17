import Data.List
import Data.Ord

main = mainB

mainA = do
  contents <- getContents
  let [ts, ss] = lines contents
  let timestamp = read ts :: Int
  let services = readServices ss
  let values =  zip services $ map (\x -> x - (timestamp `mod` x)) services
  print . (\(a, b) -> a * b) $ minimumBy (comparing snd) values

mainB = do
  contents <- getContents
  let [ts, ss] = lines contents
  let timestamp = read ts :: Int
  let servicesAndRequirements = reverse . sortBy (comparing fst) $ readServicesAndRequirements ss
  print servicesAndRequirements
  let ((t1, r1):servicesAndRemainders) = map (\(t, r) -> (t, (t-r) `mod` t)) servicesAndRequirements
  print t1
  print r1
  print servicesAndRemainders
  let mults = foldl (\mults x -> (findMultiplier mults x):mults) [(t1, r1)] servicesAndRemainders
  print mults
  print $ foldl (\x (a, b) -> x * a + b) 0 mults

splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

readServices :: String -> [Int]
readServices s = map read . filter (/= "x") $ splitOn ',' s

readServicesAndRequirements :: String -> [(Int, Int)]
readServicesAndRequirements s = map (\(x, y) -> (read x, y)) . filter ((/= "x") . fst) $ zip (splitOn ',' s) [0..]

findMultiplier :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
findMultiplier prevs (divisor, goal) =
  let
    (step, remainder) = reduceMultAndRemainder divisor prevs
    mult = fst . head . filter (\(_, x) -> x == goal) . zip [0..] $ iterate (\x -> (x + step) `mod` divisor) remainder
  in (divisor, mult)

reduceMultAndRemainder :: Int -> [(Int, Int)] -> (Int, Int)
reduceMultAndRemainder n [(a, b)] = (a `mod` n, b `mod` n)
reduceMultAndRemainder n ((m, r):rest) = 
  let 
    (m', r') = reduceMultAndRemainder n rest
  in ((m' * m) `mod` n, ((m' * r) + r') `mod` n)
