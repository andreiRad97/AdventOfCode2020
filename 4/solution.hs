import System.IO

import Data.Char

import Data.List.Split
import Data.Set (fromList, isSubsetOf, union)

import Text.Read

optionalFields = fromList ["cid"]
requiredFields = fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Entry = (String, String)
type Passport = [Entry]
type InputLine = String

accumulateLines :: [InputLine] -> InputLine -> [InputLine] -> [InputLine]
accumulateLines prevLines "" [] = prevLines
accumulateLines prevLines currLine [] = currLine : prevLines
accumulateLines prevLines currLine (x:xs) =
    case x of
      "" -> accumulateLines (currLine : prevLines) "" xs
      _ -> accumulateLines prevLines (currLine ++ " " ++ x) xs

sanitiseInput :: [InputLine] -> [InputLine]
sanitiseInput xs = accumulateLines [] "" xs

splitToEntry :: [String] -> Entry
splitToEntry [a, b] = (a, b)
splitToEntry xs = error "Cannot create entry"

parsePass :: InputLine -> Passport
parsePass ln =
    let unspacedLn = filter (/= "") $ splitOn " " ln
     in map (splitToEntry . (splitOn ":")) unspacedLn

hasFields :: Passport -> Bool
hasFields pass =
    let fields = fromList $ map fst pass
        hasReqFields = requiredFields `isSubsetOf` fields
        hasOnlyValidFields = 
            fields `isSubsetOf` (union requiredFields optionalFields)
     in hasOnlyValidFields && hasReqFields

isBoundedInt :: String -> Int -> Int -> Bool
isBoundedInt strNum lower upper =
    let value = readMaybe strNum :: Maybe Int
     in case value of
      Just i -> lower <= i && i <= upper
      Nothing -> False

isValidField :: Entry -> Bool
isValidField ("byr", value) = isBoundedInt value 1920 2002
isValidField ("iyr", value) = isBoundedInt value 2010 2020
isValidField ("eyr", value) = isBoundedInt value 2020 2030
isValidField ("hgt", value) = 
    let unit = drop (length value - 2) value
        size = take (length value - 2) value
    in case unit of
         "in" -> isBoundedInt size 59 76
         "cm" -> isBoundedInt size 150 193
         _ -> False
isValidField ("hcl", value) = 
    (length value == 7) && (head value == '#') && (all isHexDigit $ tail value)
isValidField ("ecl", value) = elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField ("pid", value) = all isDigit value && length value == 9
isValidField ("cid", _) = True
isValidField _ = False

parsePassports :: [InputLine] -> [Passport]
parsePassports inputLines = map parsePass $ sanitiseInput inputLines

countValidPassports :: String -> Int
countValidPassports input =
    let passports = parsePassports $ lines input
     in length . filter ((==) True) $ map hasFields passports

countValidPassports2 :: String -> Int
countValidPassports2 input =
    let passports = parsePassports $ lines input
     in length .filter ((==) True) $ map (\x -> hasFields x && all isValidField x) passports

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ countValidPassports contents
    putStrLn $ show $ countValidPassports2 contents
    hClose handle
