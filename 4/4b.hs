import Control.Monad
import Data.Char
import Data.List

data Passport = Passport{ byr :: Bool
                        , iyr :: Bool
                        , eyr :: Bool
                        , hgt :: Bool
                        , hcl :: Bool
                        , ecl :: Bool
                        , pid :: Bool
                        }
 
addField :: (String, String) -> Passport -> Passport
addField (key, value) passport
    | key == "byr" = passport { byr = isByrValid value } 
    | key == "iyr" = passport { iyr = isIyrValid value } 
    | key == "eyr" = passport { eyr = isEyrValid value } 
    | key == "hgt" = passport { hgt = isHgtValid value } 
    | key == "hcl" = passport { hcl = isHclValid value } 
    | key == "ecl" = passport { ecl = isEclValid value } 
    | key == "pid" = passport { pid = isPidValid value } 
    | otherwise    = passport

numericCheck :: Int -> Int -> String -> Bool
numericCheck minimum maximum strValue = 
    let value = read strValue :: Int
    in minimum <= value && value <= maximum

isByrValid = numericCheck 1920 2002
isIyrValid = numericCheck 2010 2020
isEyrValid = numericCheck 2020 2030

isHgtValid :: String -> Bool
isHgtValid value
    | isSuffixOf "cm" value = numericCheck 150 193 $ num
    | isSuffixOf "in" value = numericCheck 59 76 $ num
    | otherwise             = False
    where num = take (length value - 2) value

isHclValid :: String -> Bool
isHclValid (head:tail) = (head == '#') && length tail == 6 && (and $ map (has (['0'..'9']++['a'..'f'])) tail)

isEclValid :: String -> Bool
isEclValid = has ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isPidValid :: String -> Bool
isPidValid value = length value == 9 && (and $ map (has ['0'..'9']) value)

valid :: Passport -> Bool
valid passport = (byr passport &&
                  iyr passport &&
                  eyr passport &&
                  hgt passport &&
                  hcl passport &&
                  ecl passport &&
                  pid passport)

newPassport :: Passport
newPassport = Passport False False False False False False False

main = do
  contents <- getContents
  print . countValid .lines $ contents

countValid :: [String] -> Int
countValid lines = countValid' lines 0 newPassport

countValid' :: [String] -> Int -> Passport -> Int
countValid' [] count pass = count + (fromEnum . valid $ pass)
countValid' (line:rest) count passport
    | length fields == 0 = countValid' rest (count + (fromEnum . valid $ passport)) newPassport
    | otherwise          = countValid' rest count updatedPassport
    where
      fields = getFields line
      updatedPassport = foldr addField passport fields

getFields :: String -> [(String, String)]
getFields = map getKeyValue . words

getKeyValue :: String -> (String, String)
getKeyValue field = (key, value)
  where
    key = takeWhile (/=':') $ field
    value = tail . dropWhile (/=':') $ field

has :: Eq a => [a] -> a -> Bool
has = flip elem
