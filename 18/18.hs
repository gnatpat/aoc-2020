data Expr = Empty | Mult Expr' Expr deriving (Show)
data Expr' = Empty'  | Add Value Expr' deriving (Show)
data Value = Value Int | Brackets Expr deriving (Show)

main = mainB

mainA = do
  answer <- sum . map eval . lines <$> getContents
  print answer

mainB = do
  answer <- sum . map eval2 . lines <$> getContents
  print answer

eval :: String -> Int
eval = fst . eval' . slurpValue1 . tokenise

tokenise :: String -> [String]
tokenise = words . addSpaces

addSpaces :: String -> String
addSpaces [] = []
addSpaces ('(':xs) = '(':' ':(addSpaces xs)
addSpaces (')':xs) = ' ':')':(addSpaces xs)
addSpaces (x:xs)   = x:(addSpaces xs)

eval' :: (Int, [String]) -> (Int, [String])
eval' (acc, []) = (acc, [])
eval' (acc, (")":rest)) = (acc, rest)
eval' (acc, (op:rest)) =
  let
    (value, rest') = slurpValue1 rest
    f = opF op
    acc' = f acc value
  in eval' (acc', rest')

slurpValue1 :: [String] -> (Int, [String])
slurpValue1 ("(":xs) = eval' (slurpValue1 xs)
slurpValue1 (x:xs) = (read x, xs)

    
opF :: String -> (Int -> Int -> Int)
opF "+" = (+)
opF "*" = (*)

eval2 :: String -> Int
eval2 = evalExpr . parseExpr . tokenise 

slurp :: (Eq a, Show a) => a -> [a] -> [a]
slurp v (x:xs)
  | v == x    = xs
  | otherwise = error ("Cannot slurp '" ++ (show v) ++ "', got '" ++ (show x) ++ "' instead")

parseExpr :: [String] -> Expr
parseExpr [] = Empty
parseExpr xs
  | rest == [] = Mult expr' Empty
  | otherwise  = Mult expr' . parseExpr $ (slurp "*" rest)
  where
    (expr', rest) = slurpExpr' xs
    

slurpExpr' :: [String] -> (Expr', [String])
slurpExpr' [] = (Empty', [])
slurpExpr' xs
  | rest == [] = (Add value Empty', rest)
  | head rest == "+" = (Add value otherExpr', rest')
  | otherwise  = (Add value Empty', rest)
  where
    (value, rest) = slurpValue xs
    (otherExpr', rest') = slurpExpr' (slurp "+" rest)

slurpValue :: [String] -> (Value, [String])
slurpValue ("(":xs) =
  let
    (exprValues, rest) = splitOnClosingBracket xs
  in (Brackets (parseExpr exprValues), rest)
slurpValue (x:xs) = (Value (read x), xs)

splitOnClosingBracket = splitOnClosingBracket' 0

splitOnClosingBracket' :: Int -> [String] -> ([String], [String])
splitOnClosingBracket' 0 (")":xs) = ([], xs)
splitOnClosingBracket' n (x:xs) =
  let 
    newN = if x == "(" then n+1 else (if x == ")" then n-1 else n)
    (a, b) = splitOnClosingBracket' newN xs 
  in
    (x:a, b)

evalExpr :: Expr -> Int
evalExpr Empty = 1
evalExpr (Mult expr' expr) = (evalExpr' expr') * (evalExpr expr)

evalExpr' :: Expr' -> Int
evalExpr' Empty' = 0
evalExpr' (Add value expr') = (evalValue value) + (evalExpr' expr')

evalValue :: Value -> Int
evalValue (Value x) = x
evalValue (Brackets expr) = evalExpr expr
