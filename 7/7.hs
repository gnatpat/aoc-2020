import Data.List

main = mainB

mainA = do
  contents <- getContents
  let bags = map lineToBag . lines $ contents
  let containers = canContain bags "shiny gold"
  print $ length containers

mainB = do
  contents <- getContents
  let bags = map lineToBag . lines $ contents
  print $ countContainedBags bags "shiny gold"

data BagRule = BagRule { name :: String
                       , rules :: [String]
                       , counts :: [Int]
                       } deriving (Show)

lineToBag :: String -> BagRule
lineToBag line =
  let
    parts = words line
    bag = unwords $ take 2 parts
    contents = getRules (drop 4 parts)
    counts = map fst contents
    rules = map snd contents
  in
    BagRule bag rules counts
    
getRules :: [String] -> [(Int, String)]
getRules [] = []
getRules ["no", "other", "bags."] = []
getRules (count:mod:col:_:xs) = (read count , unwords [mod, col]) : getRules xs

findOuters :: [BagRule] -> String -> [String]
findOuters [] _ = []
findOuters (x:xs) searchName
  | searchName `elem` rules x = name x : findOuters xs searchName
  | otherwise                 = findOuters xs searchName

findRule :: [BagRule] -> String -> BagRule
findRule [] _ = error "Missing bag"
findRule (x:xs) bag
  | bag == name x = x
  | otherwise     = findRule xs bag

canContain :: [BagRule] -> String -> [String]
canContain rules bag = 
  let
    containingBags = findOuters rules bag
    outer = concat $ map (canContain rules) containingBags
    all = nub (outer ++ containingBags)
  in all

countContainedBags :: [BagRule] -> String -> Int
countContainedBags allRules bag =
  let
    rule = findRule allRules bag
    bagAndCount = zip (rules rule) (counts rule)
  in
    sum $ map (\(r, c) -> (countContainedBags allRules r) * c + c) bagAndCount
