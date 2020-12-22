import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Food = Food { ingredients :: [String], allergens :: [String] } deriving (Show, Eq)

main = do
  foods <- map parseFood . lines <$> getContents
  let
    allergenCandidates = foldr updateAllergenCandidates Map.empty foods
    potentialAllergens = nub . concat $ Map.elems allergenCandidates
    solved = zip (map fst $ Map.toList allergenCandidates) $ solve potentialAllergens (map snd $ Map.toList allergenCandidates)
  print allergenCandidates
  print . length . filter (flip notElem potentialAllergens) . concat $ map ingredients foods
  print solved
  putStrLn . foldl1 (++) . intersperse "," $ map snd solved

parseFood :: String -> Food
parseFood line =
  let
    parts = words line
    [ingredients, contains] = splitOn "(contains" parts
    allergens = map init contains
  in (Food ingredients allergens)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

updateAllergenCandidates :: Food -> Map String [String] -> Map String [String]
updateAllergenCandidates (Food ingredients allergens) candidates =
    foldr updateAllergen candidates . zip allergens $ repeat ingredients

updateAllergen ::  (String, [String]) -> Map String [String] -> Map String [String]
updateAllergen = uncurry $ Map.insertWith intersect


solve :: [String] -> [[String]] -> [String]
solve [] options = map head options
solve values options = solve values' options'
  where
    solved = head . head $ filter (\x -> (elem (head x) values) && length x == 1) options
    options' = map (\o -> if o /= [solved] then removeItem solved o else o) options
    values' = removeItem solved values

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
