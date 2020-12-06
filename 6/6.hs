import Data.List

main = mainB

mainA = do
  contents <- getContents
  print . sum . map (length . anyAnswer) . chunk [] . lines $ contents

mainB = do
  contents <- getContents
  print . sum . map (length . allAnswered) . chunk [] . lines $ contents

chunk :: [String] -> [String] -> [[String]]
chunk [] [] = []
chunk c [] = [c]
chunk c ("":xs) = c : (chunk [] xs)
chunk c (x:xs) = chunk (x : c) xs
  
anyAnswer :: [String] -> String
anyAnswer = nub . concat

allAnswered :: [[Char]] -> [Char]
allAnswered = foldr1 intersect 
