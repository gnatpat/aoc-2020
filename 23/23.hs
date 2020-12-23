import Data.Char
import Data.IntMap.Strict (IntMap)
import Data.List
import qualified Data.IntMap.Strict as IntMap

main = mainB
mainA = do
  values <- map digitToInt <$> getLine
  let nextLookup = IntMap.fromList $ zip values (take (length values) . drop 1 $ cycle values)
      first = head values
      (lastLookup, lastCurrent) = iterate' (move 9) (nextLookup, first) !! 100
  putStrLn . foldr (++) "" . map show . take 8 . tail $ iterate ((IntMap.!) lastLookup) 1

mainB = do
  values <- map digitToInt <$> getLine
  let allValues = values ++ [10..1000000]
      nextLookup = IntMap.fromList $ zip allValues (take (length allValues) . drop 1 $ cycle allValues)
      first = head values
      (lastLookup, _) = foldl' (\a b -> move 1000000 a) (nextLookup, first) [1..10000000]
  print . product . take 2 . tail $ iterate ((IntMap.!) lastLookup) 1

move :: Int -> (IntMap Int, Int) -> (IntMap Int, Int)
move maxValue (nextLookup, x) =
  let
    next4 = take 4 . tail $ iterate ((IntMap.!) nextLookup) x
    removed = take 3 next4
    next = last next4
    nextLookup' = IntMap.insert x next nextLookup
    dest = head . filter (flip notElem removed) $ map (\x -> if x < 1 then x + maxValue else x) [x-1,x-2..]
    afterDest = nextLookup' IntMap.! dest
    nextLookup'' = IntMap.insert (last removed) afterDest $ IntMap.insert dest (head removed) nextLookup'
  in (nextLookup'', next)
