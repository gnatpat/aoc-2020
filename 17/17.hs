import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int, Int, Int)

mainA = main' 3
mainB = main' 4

main = mainB

main' x = do
  alive <- readInput <$> getContents
  print . length $ iterate (step x) alive !! 6

readRow :: (Int, String) -> [(Int, Int)]
readRow (y, s) = map (\x -> (fst x, y)) . filter ((=='#') . snd) . zip [0..] $ s

readInput :: String -> Set Coord
readInput = Set.fromList . map (\(x,y) -> (x, y, 0, 0)) . concat . map readRow . zip [0..] . lines

getAroundCoords3 :: Coord -> [Coord]
getAroundCoords3 (x, y, z, w) = [((x+xx), (y+yy), (z+zz), w) | xx <- [-1..1],
                                                              yy <- [-1..1],
                                                              zz <- [-1..1],
                                                              not (xx == 0 && yy == 0 && zz == 0)]

getAroundCoords4 :: Coord -> [Coord]
getAroundCoords4 (x, y, z, w) = [((x+xx), (y+yy), (z+zz), w+ww) | xx <- [-1..1],
                                                                  yy <- [-1..1],
                                                                  zz <- [-1..1],
                                                                  ww <- [-1..1],
                                                                  not (xx == 0 && yy == 0 && zz == 0 && ww == 0)]

getAroundCoords :: Int -> Coord -> [Coord]
getAroundCoords 3 = getAroundCoords3
getAroundCoords 4 = getAroundCoords4

getAroundCoordsAndSelf :: Int -> Coord -> [Coord]
getAroundCoordsAndSelf dim c = c:(getAroundCoords dim c)

candidates :: Int -> Set Coord -> Set Coord 
candidates dim = Set.fromList . concat . Set.map (getAroundCoordsAndSelf dim)

aliveNeighbours :: Int -> Set Coord -> Coord -> Int
aliveNeighbours dim alive c = length . filter (\c -> Set.member c alive) $ getAroundCoords dim c

stepPoint :: Int -> Set Coord -> Coord -> Bool
stepPoint dim alive c
  | isAlive   = (neighbourCount == 2) || (neighbourCount == 3)
  | otherwise = (neighbourCount == 3)
  where
    neighbourCount = aliveNeighbours dim alive c
    isAlive        = Set.member c alive

step :: Int -> Set Coord -> Set Coord
step dim alive = Set.filter (\c -> stepPoint dim alive c) $ candidates dim alive
