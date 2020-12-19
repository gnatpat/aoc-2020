import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data State = State { history :: IntMap Int,
                     prev :: Int } deriving Show

mainA = main' 2020
mainB = main' 30000000

main = mainB

main' x = do
  contents <- getContents
  let initialValues = map (read) $ splitOn ',' contents
  print . prev $ foldl' step (initState initialValues) [(length initialValues)..x-1]


splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

step :: State -> Int -> State
step (State history prev) index = State updatedHistory (index-prevPrevIndex)
  where
    updatedHistory = IntMap.insert prev index history
    prevPrevIndex  = IntMap.findWithDefault index prev history

initState :: [Int] -> State
initState nums = 
  let
    history = IntMap.fromList $ zip (init nums) [1..]
  in State history (last nums)
