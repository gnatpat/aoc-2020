main = mainB

mainA = do
  score <- playCombat . parse <$> getContents
  print score

mainB = do
  outcome <- playRecursiveCombat . parse <$> getContents
  print outcome


parse :: String -> ([Int], [Int])
parse s = tuplify . map (map read) . map tail . splitOn "" $ lines s

tuplify :: [[Int]] -> ([Int], [Int])
tuplify [x,y] = (x, y)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

playCombat :: ([Int], [Int]) -> Int
playCombat ([], x) = scoreHand x
playCombat (x, []) = scoreHand x
playCombat ((x:xs), (y:ys))
  | x > y = playCombat (xs ++ [x, y], ys)
  | y > x = playCombat (xs, ys ++ [y, x])

scoreHand :: [Int] -> Int
scoreHand = sum . zipWith (*) [1..] . reverse

playRecursiveCombat :: ([Int], [Int]) -> (Winner, Int)
playRecursiveCombat = playRecursiveCombat' []

data Winner = PLAYER1 | PLAYER2 deriving (Show, Eq)

playRecursiveCombat' :: [([Int], [Int])] -> ([Int], [Int]) -> (Winner, Int)
playRecursiveCombat' _ ([], x) = (PLAYER2, scoreHand x)
playRecursiveCombat' _ (x, []) = (PLAYER1, scoreHand x)
playRecursiveCombat' history h@(p1@(x:xs), (y:ys))
  | h `elem` history                 = (PLAYER1, scoreHand p1)
  | x <= length xs && y <= length ys = playRecursiveCombat' history' (giveCardsToWinner recursiveWinner h)
  | otherwise                        = playRecursiveCombat' history' (giveCardsToWinner normalWinner h)
  where
    (recursiveWinner, _) = playRecursiveCombat (take x xs, take y ys)
    history' = h:history
    normalWinner = if x > y then PLAYER1 else PLAYER2

giveCardsToWinner :: Winner -> ([Int], [Int]) -> ([Int], [Int])
giveCardsToWinner winner ((x:xs), (y:ys))
  | winner == PLAYER1 = (xs ++ [x, y], ys)
  | winner == PLAYER2 = (xs, ys ++ [y, x])
