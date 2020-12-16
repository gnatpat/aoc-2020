import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes)

data State = FLOOR | EMPTY | OCCUPIED deriving (Show, Eq)

data Board = Board { tiles :: IntMap State
                   , sights :: IntMap [Coords] -- Sights is which tiles are looked at by each tile
                   , width :: Int
                   , height :: Int
                   } deriving (Show, Eq)

type Coords = (Int, Int)
type StepFunction = Board -> Coords -> State
type SightFunction = Board -> Coords -> [Coords]

main = mainB
mainA = do
  contents <- getContents
  print . countOccupied . stepUntilStable (stepTile 4) . readBoard adjacentSight $ contents

mainB = do
  contents <- getContents
  print . countOccupied . stepUntilStable (stepTile 5) . readBoard linearSight $ contents

readTile :: Char -> State
readTile '.' = FLOOR
readTile 'L' = EMPTY
readTile '#' = OCCUPIED

readBoard :: SightFunction -> String -> Board
readBoard f contents = 
  let
    tiles = map readTile . concat $ lines contents
    width = length (head (lines contents))
    height = length (lines contents)
    mapTiles = IntMap.fromAscList $ zip [0..] tiles
    preboard = Board mapTiles IntMap.empty width height
    sights = IntMap.fromAscList $ zip [0..] $ map (f preboard) [(x, y) | y <- [0..height-1], x <- [0..width-1]]
  in Board mapTiles sights width height

countOccupied :: Board -> Int
countOccupied (Board tiles _ _ _) = length . filter (== OCCUPIED) $ IntMap.elems tiles

getTile :: Board -> Coords -> State
getTile (Board tiles _ width _) (x, y) = tiles IntMap.! (y * width + x)

getSight :: Board -> Coords -> [Coords]
getSight (Board _ sights width _) (x, y) = sights IntMap.! (y * width + x)

countOccupiedSeen :: Board -> Coords -> Int
countOccupiedSeen b@(Board _ sights _ _) (x, y) = 
  length . filter (==OCCUPIED) . map (getTile b) $ getSight b (x, y)

stepTile :: Int -> StepFunction
stepTile x board coords
  | tile == EMPTY && occupiedAround == 0    = OCCUPIED
  | tile == OCCUPIED && occupiedAround >= x = EMPTY
  | otherwise                               = tile
  where occupiedAround = countOccupiedSeen board coords
        tile = getTile board coords

stepBoard :: StepFunction -> Board -> Board
stepBoard f board@(Board tiles sights width height) =
  let
    newTiles = IntMap.fromAscList $ zip [0..] [f board (x, y) | y <- [0..height-1], x <- [0..width-1]]
  in (Board newTiles sights width height)

stepUntilStable :: StepFunction -> Board -> Board
stepUntilStable f board
  | nextBoard == board = board
  | otherwise          = stepUntilStable f nextBoard
  where nextBoard = stepBoard f board

adjacentSight :: SightFunction
adjacentSight board (x, y) = filter (inBounds board) [(x+xx, y+yy) | xx <- [-1..1],
                                                                       yy <- [-1..1],
                                                                       not (xx == 0 && yy ==0)]

linearSight :: SightFunction
linearSight board (x, y) = catMaybes [findSeatInDirection board (x, y) (xx, yy) | xx <- [-1..1],
                                                                                  yy <- [-1..1],
                                                                                  not (xx == 0 && yy ==0)]

findSeatInDirection :: Board -> Coords -> Coords -> Maybe Coords
findSeatInDirection board (x, y) (dx, dy)
  | not $ inBounds board next = Nothing
  | tile /= FLOOR             = Just next
  | otherwise                 = findSeatInDirection board next (dx, dy)
  where next = (x + dx, y + dy)
        tile = getTile board next

inBounds :: Board -> Coords -> Bool
inBounds (Board _ _ width height) (x, y) = x >= 0 && x < width && y >= 0 && y < height
