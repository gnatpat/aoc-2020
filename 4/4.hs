import Control.Monad
import Data.Char

data Passport = Passport{ byr :: Bool
                        , iyr :: Bool
                        , eyr :: Bool
                        , hgt :: Bool
                        , hcl :: Bool
                        , ecl :: Bool
                        , pid :: Bool
                        , cid :: Bool
                        }
 
addField :: String -> Passport -> Passport
addField field passport
    | field == "byr" = passport { byr = True } 
    | field == "iyr" = passport { iyr = True } 
    | field == "eyr" = passport { eyr = True } 
    | field == "hgt" = passport { hgt = True } 
    | field == "hcl" = passport { hcl = True } 
    | field == "ecl" = passport { ecl = True } 
    | field == "pid" = passport { pid = True } 
    | field == "cid" = passport { cid = True } 
    | otherwise      = passport

valid :: Passport -> Bool
valid passport = (byr passport &&
                  iyr passport &&
                  eyr passport &&
                  hgt passport &&
                  hcl passport &&
                  ecl passport &&
                  pid passport)

newPassport :: Passport
newPassport = Passport False False False False False False False False

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

getFields :: String -> [String]
getFields = map (takeWhile (/= ':')) . words
