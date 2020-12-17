type Coords = (Int, Int)
instance (Num a,Num b) => Num (a, b) where
   (a,b) + (c,d) = (a+c,b+d)
   (a,b) * (c,d) = (a*c,b*d)
   (a,b) - (c,d) = (a-c,b-d)
   abs    ((a,b)) = (abs a,    abs b) 
   signum ((a,b)) = (signum a, signum b) 
   fromInteger i = (fromInteger i, fromInteger i)

data Ship = Ship { facing :: Int
                 , location :: Coords
                 } deriving (Show)

data ShipW = ShipW { location' :: Coords
                   , waypoint :: Coords
                   } deriving (Show)

data Action = NORTH | EAST | SOUTH | WEST | LEFT | RIGHT | FORWARDS

data Instruction = Instruction Action Int

main = mainB

mainA = do
  contents <- getContents
  print . (\(a, b) -> a+b) . abs . location . foldl applyAction (Ship 0 (0, 0)) . map parseInstruction . lines $ contents

mainB = do
  contents <- getContents
  print . (\(a, b) -> a+b) . abs . location' . foldl applyAction' (ShipW (0, 0) (10, 1)) . map parseInstruction . lines $ contents

parseAction :: Char -> Action
parseAction 'N' = NORTH
parseAction 'E' = EAST
parseAction 'S' = SOUTH
parseAction 'W' = WEST
parseAction 'L' = LEFT
parseAction 'R' = RIGHT
parseAction 'F' = FORWARDS

parseInstruction :: String -> Instruction
parseInstruction (a:value) = Instruction (parseAction a) (read value)

applyAction :: Ship -> Instruction -> Ship
applyAction (Ship facing (x, y)) (Instruction NORTH value) = Ship facing (x, y + value)
applyAction (Ship facing (x, y)) (Instruction SOUTH value) = Ship facing (x, y - value)
applyAction (Ship facing (x, y)) (Instruction EAST value) = Ship facing (x + value, y)
applyAction (Ship facing (x, y)) (Instruction WEST value) = Ship facing (x - value, y)
applyAction (Ship facing location) (Instruction FORWARDS value) = Ship facing (location + (facingToDirection facing) * (value, value))
applyAction (Ship facing location) (Instruction LEFT value) = Ship (facing - value) location
applyAction (Ship facing location) (Instruction RIGHT value) = Ship (facing + value) location

facingDirections :: [Coords]
facingDirections = [(1, 0), (0, -1), (-1, 0), (0, 1)]

facingToDirection :: Int -> Coords
facingToDirection facing
  | facing >= 360 = facingToDirection (facing - 360)
  | facing < 0    = facingToDirection (facing + 360)
  | otherwise     = facingDirections !! (facing `div` 90)

applyAction' :: ShipW -> Instruction -> ShipW
applyAction' (ShipW c (wx, wy)) (Instruction NORTH value) = ShipW c (wx, wy + value)
applyAction' (ShipW c (wx, wy)) (Instruction SOUTH value) = ShipW c (wx, wy - value)
applyAction' (ShipW c (wx, wy)) (Instruction EAST value) = ShipW c (wx + value, wy)
applyAction' (ShipW c (wx, wy)) (Instruction WEST value) = ShipW c (wx - value, wy)
applyAction' (ShipW c w) (Instruction FORWARDS value) = ShipW (c + w * (value, value)) w
applyAction' (ShipW c (wx, wy)) (Instruction LEFT 90) = ShipW c (-wy, wx)
applyAction' (ShipW c (wx, wy)) (Instruction LEFT 180) = ShipW c (-wx, -wy)
applyAction' (ShipW c (wx, wy)) (Instruction LEFT 270) = ShipW c (wy, -wx)
applyAction' (ShipW c (wx, wy)) (Instruction RIGHT 90) = ShipW c (wy, -wx)
applyAction' (ShipW c (wx, wy)) (Instruction RIGHT 180) = ShipW c (-wx, -wy)
applyAction' (ShipW c (wx, wy)) (Instruction RIGHT 270) = ShipW c (-wy, wx)

