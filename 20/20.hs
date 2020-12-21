import Data.Maybe
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Picture = Picture { pictureData :: PictureData,
                         index :: Int, signature :: Signature } deriving Show
type PictureData = [String]

-- East South West North
type Sides = [String] 
type Signature = [Sides]

data OrientedPiece = OrientedPiece { pIndex :: Int, flipped :: Bool, rotation :: Int } deriving (Show, Eq)

monster :: [String]
monster = ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
monsterWidth = length $ head monster
monsterHeight = length monster

monsterPlaces :: [(Int, Int)]
monsterPlaces = concat . map (\(y, xs) -> map (\x -> (x, y)) xs) . zip [0..] $ map (map fst . filter ((== '#') . snd) . zip [0..]) monster

main = do
  pictures <-  map toPicture . splitOn "" . lines <$> getContents
  let corners = map head . filter ((==4) . length) . group . sort . Map.elems . foldr flipEntry Map.empty . concat $ map getSideToIndex pictures
  print $ product corners
  let sideLookup = Map.fromListWith (++) . map (\(a, b) -> (a, [b])) . concat $ map getSideToIndex pictures
      pictureLookup = IntMap.fromList $ map (\p -> (index p, p)) pictures
      firstCorner = pictureLookup IntMap.! (head corners)
      topLeftSides = head $ signature firstCorner
      y = getFirstPieceOrientation . zip topLeftSides $ map (other (index firstCorner) . (sideLookup Map.!)) topLeftSides
      firstColumn = solveRow (-1) 3 y sideLookup pictureLookup
      solution = map (\x -> solveRow (-1) 2 x sideLookup pictureLookup) $ map ((!! 2) . getSides pictureLookup) firstColumn
      picture = concat . map (map concat) . map transpose $ map (map (stripBorder . orientPicture pictureLookup)) solution
      seaMonsters = maximum . map length . map findMonsters $ potentialPictures picture
      hashes = length . filter (=='#') $ concat picture
      seaMonsterHashes = seaMonsters * length monsterPlaces
  print $ hashes - seaMonsterHashes

potentialPictures :: [String] -> [[String]]
potentialPictures p = map (movePicture p) $ (,) <$> [0..3] <*> [False, True]

movePicture :: [String] -> (Int, Bool) -> [String]
movePicture p (0, False) = p
movePicture p (0, True) = map reverse p
movePicture p (n, flip) = rotatePicture (movePicture p (n-1, flip))

findMonsters :: [String] -> [(Int, Int)]
findMonsters picture =
  let
    width = length $ head picture
    height = length picture
    potentialMonsterOrigins = [(x, y) | x <- [0..(width-monsterWidth)], y <- [0..(height-monsterHeight)]]
    monsterOrigins = filter (isMonsterAt picture) potentialMonsterOrigins
  in monsterOrigins

isMonsterAt :: [String] -> (Int, Int) -> Bool
isMonsterAt picture (x, y) = all (=='#') . map (\(x, y) -> picture !! y !! x) $ map (\(x', y') -> (x+x', y+y')) monsterPlaces

stripBorder :: [String] -> [String]
stripBorder = map (init . tail) . init . tail

orientPicture :: IntMap Picture -> OrientedPiece -> [String]
orientPicture pictureLookup (OrientedPiece pIndex flipped rotation) =
  let
    pData = pictureData $ pictureLookup IntMap.! pIndex
    pData' = if flipped then map reverse pData else pData
  in iterate rotatePicture pData' !! rotation

rotatePicture :: [[a]] -> [[a]]
rotatePicture = map reverse . transpose


getFirstPieceOrientation :: [(String, Maybe a)] -> String
getFirstPieceOrientation [(_, Nothing), (y, Nothing), _, _] = y
getFirstPieceOrientation [_, (_, Nothing), (y, Nothing), _] = y
getFirstPieceOrientation [_, _, (_, Nothing), (y, Nothing)] = y
getFirstPieceOrientation [(y, Nothing), _, _, (_, Nothing)] = y

getSides :: IntMap Picture -> OrientedPiece -> [String]
getSides pictureLookup (OrientedPiece i flipped rotation) =
  let
    picture = pictureLookup IntMap.! i
    sides = (signature picture) !! (fromEnum flipped)
  in take 4 . drop (4-rotation) $ cycle sides

solveRow :: Int -> Int -> String -> Map String [Int] -> IntMap Picture -> [OrientedPiece]
solveRow index sideDir sideToMatch sideLookup pictureLookup
  | match == Nothing = []
  | otherwise        = match':(solveRow (pIndex match') sideDir nextSideToMatch sideLookup pictureLookup)
  where
    match = findMatchedPiece index sideDir sideToMatch sideLookup pictureLookup
    match' = fromJust match
    nextSideToMatch = reverse $ getOppositeSide pictureLookup match' sideDir

findMatchedPiece :: Int -> Int -> String -> Map String [Int] -> IntMap Picture -> Maybe OrientedPiece
findMatchedPiece index sideDir sideToMatch sideLookup pictureLookup
  | match == Nothing = Nothing
  | otherwise        = Just orientedPiece
  where
    match = other index $ sideLookup Map.! (reverse sideToMatch)
    orientedPiece = orientPiece sideDir sideToMatch (pictureLookup IntMap.! fromJust match)

getOppositeSide :: IntMap Picture -> OrientedPiece -> Int -> String
getOppositeSide pictureLookup o sideDir = cycle (getSides pictureLookup o) !! (sideDir+2)

orientPiece :: Int -> String -> Picture -> OrientedPiece
orientPiece sideDir matchedSide (Picture _ i s) =
  let
    flipped = (matchedSide `elem` (s !! 1))
    side = s !! (fromEnum flipped)
    rotation = (sideDir - (fromJust $ elemIndex matchedSide side) + 4) `mod` 4
  in OrientedPiece i flipped rotation

other :: Eq a => a -> [a] -> Maybe a
other _ [] = Nothing
other a [x] = if a == x then Nothing else Just x
other a [x,y] = if a == x then Just y else Just x

flipEntry :: (Ord k) => (k, v) -> Map k v -> Map k v
flipEntry (k, v) m
  | Map.member k m = Map.delete k m
  | otherwise      = Map.insert k v m

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

toPicture :: [String] -> Picture
toPicture (idS:pdata) = Picture pdata (read . init $ words idS !! 1) (getSignature pdata)

getSignature :: PictureData -> Signature
getSignature p = [[getRight p,
                   reverse $ getBottom p,
                   reverse $ getLeft p,
                   getTop p],
                  [getLeft p,
                   getBottom p,
                   reverse $ getRight p,
                   reverse $ getTop p]]

getTop :: PictureData -> String
getTop = head

getLeft :: PictureData -> String
getLeft = map head

getRight :: PictureData -> String
getRight = map last

getBottom :: PictureData -> String
getBottom = last

getSideToIndex :: Picture -> [(String, Int)]
getSideToIndex p = map (\s -> (s, (index p))) . concat $ signature p
