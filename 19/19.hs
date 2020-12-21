import Control.Monad
import Data.List
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

data Rule = Match Char | Options [[Int]] deriving Show

main = do
  (idAndRules, candidates) <- slurpRules . lines <$> getContents
  let rules = IntMap.fromList idAndRules
  print . length . filter ((elem "") . snd) . zip candidates $ map (\c -> slurpRule rules [c] (rules IntMap.! 0)) candidates

slurpRules :: [String] -> ([(Int, Rule)], [String])
slurpRules ("":xs) = ([], xs)
slurpRules (rs:xs) = ((parseRuleLine rs):rules, rest)
  where (rules, rest) = slurpRules xs

parseRuleLine :: String -> (Int, Rule)
parseRuleLine s =
  let
    [ruleIdS, rulesS] = splitOn ':' s
    ruleId = read ruleIdS
  in (ruleId, parseRule . words $ rulesS)
    
parseRule :: [String] -> Rule
parseRule xs
  | (head $ head xs) == '"' = Match (head xs !! 1)
  | otherwise = Options (map (map read) $ splitOn "|" xs)


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

slurpRule :: IntMap Rule -> [String] -> Rule -> [String]
slurpRule _ [] _ = []
slurpRule _ candidates (Match c) = map tail $ filter (isPrefixOf [c]) candidates
slurpRule rules s (Options options) = concat $ map (foldl (slurpRule rules) s . map (rules IntMap.!)) options
