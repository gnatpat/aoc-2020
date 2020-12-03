import Control.Monad
import Data.Char

main = do
  contents <- getContents
  let count = length . filter (uncurry isTree) . (zip [0..]) . lines $ contents
  print count

isTree :: Int -> String -> Bool
isTree x s =
  let xx = mod (x * 3) (length s)
      char = s !! xx
  in char == '#'
