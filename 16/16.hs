import Data.List

data Field = Field { name :: String, predicate :: Predicate } deriving Show

type Ticket = [Int]
type Predicate = ((Int, Int), (Int, Int))

main = mainB

mainA = do
  (fields, _, tickets) <- parseInput <$> getContents
  let predicates = map predicate fields
  print . sum . filter (not . matchesAnyPred predicates) $ concat tickets

mainB = do
  (fields, myTicket, dirtyTickets) <- parseInput <$> getContents
  let predicates = map predicate fields
  let tickets = filter (isValid predicates) dirtyTickets
  let fieldValues = transpose tickets
  let validFields = map (getValidFields fields) fieldValues
  let fieldNames = solve (map name fields) validFields
  print . product . map snd . filter (isPrefixOf "departure" . fst) $ zip fieldNames myTicket

parseInput :: String -> ([Field], Ticket, [Ticket])
parseInput input =
  let
    ls = lines input
    (fields, rest) = parseFields ls
    myTicket = readTicket . head $ drop 1 rest
    ticketLines = drop 4 rest
    otherTickets = map readTicket ticketLines
  in (fields, myTicket, otherTickets)
    
readTicket :: String -> [Int]
readTicket = map read . splitOn ','

parseFields :: [String] -> ([Field], [String])
parseFields ("":xs) = ([], xs)
parseFields (l:xs) = 
  let
    (fields, rest) = parseFields xs
    [name, predicatesS] = splitOn ':' l
    predicates = parsePredicates predicatesS
    field = Field name predicates
  in (field:fields, rest)

parsePredicates :: String -> Predicate
parsePredicates l =
  let
    [p1, _, p2] = words l
    [pa1, pb1] = map read $ splitOn '-' p1
    [pa2, pb2] = map read $ splitOn '-' p2
  in ((pa1, pb1), (pa2, pb2))

splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn c (cc:rest)
  | c == cc = []:current:others
  | otherwise = (cc:current):others
  where (current:others) = splitOn c rest

matchesPred :: Predicate -> Int -> Bool
matchesPred ((l1, u1), (l2, u2)) x = (l1 <= x && u1 >= x) || (l2 <= x && u2 >= x)

matchesAnyPred :: [Predicate] -> Int -> Bool
matchesAnyPred preds = or . sequenceA (map matchesPred preds)

isValid :: [Predicate] -> Ticket -> Bool
isValid predicates = not . any (not . matchesAnyPred predicates)

getValidFields :: [Field] -> [Int] -> [String]
getValidFields [] _ = []
getValidFields ((Field name pred):xs) values
  | all (matchesPred pred) values = name:(getValidFields xs values)
  | otherwise                     = getValidFields xs values

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
  
solve :: [String] -> [[String]] -> [String]
solve [] options = map head options
solve values options = solve values' options'
  where
    solved = head . head $ filter (\x -> (elem (head x) values) && length x == 1) options
    options' = map (\o -> if o /= [solved] then removeItem solved o else o) options
    values' = removeItem solved values
