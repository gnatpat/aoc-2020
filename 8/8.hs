main = mainB

mainA = do
  contents <- getContents
  let instructions = map parseInstruction $ lines contents
  print $ executeUntilLoop instructions (State 0 0) []

mainB = do
  contents <- getContents
  let instructions = map parseInstruction $ lines contents
  print $ executeUntilTermination instructions (State 0 0) [] False
   
data State = State { pc :: Int, acc :: Int }
data Instruction = Acc Int | Jmp Int | Nop Int

parseInstruction :: String -> Instruction
parseInstruction line
  | str == "acc" = Acc value
  | str == "jmp" = Jmp value
  | str == "nop" = Nop value
  where
    [str, strValue] = words line
    strValue' = if head strValue == '+' then drop 1 strValue else strValue
    value = read strValue'

executeUntilLoop :: [Instruction] -> State -> [Int] -> Int
executeUntilLoop program state visited
  | pc nextState `elem` visited = acc state
  | otherwise                   = executeUntilLoop program nextState ((pc nextState):visited)
  where
    nextState = execute state (program !! (pc state))

executeUntilTermination :: [Instruction] -> State -> [Int] -> Bool -> (Bool, Int)
executeUntilTermination program state visited haveFlipped
  | pc state == length program  = (True, acc state)
  | pc state `elem` visited     = (False, -1)
  | not haveFlipped             = combine nextResult nextFlippedResult
  | otherwise                   = nextResult
  where
    currentInstruction = program !! (pc state)
    nextVisited = (pc state):visited
    nextState = execute state currentInstruction
    nextFlippedState = execute state (flipInstruction currentInstruction)
    nextResult = executeUntilTermination program nextState nextVisited haveFlipped
    nextFlippedResult = executeUntilTermination program nextFlippedState nextVisited True

combine :: (Bool, Int) -> (Bool, Int) -> (Bool, Int)
combine (False, _) (False, _) = (False, -1)
combine (True, v) _ = (True, v)
combine _ y = y

flipInstruction :: Instruction -> Instruction
flipInstruction (Jmp value) = Nop value
flipInstruction (Nop value) = Jmp value
flipInstruction (Acc value) = Acc value

execute :: State -> Instruction -> State
execute (State pc acc) (Acc arg) = State (pc + 1) (acc + arg)
execute (State pc acc) (Jmp arg) = State (pc + arg) acc
execute (State pc acc) (Nop arg) = State (pc + 1) acc
