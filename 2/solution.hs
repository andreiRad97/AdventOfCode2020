import Data.Bits
import Data.List.Split
import System.IO

data Entry = Entry { minOccurs :: Int
                   , maxOccurs :: Int
                   , char :: Char
                   , password :: String
                   } deriving Show

parseEntry :: String -> Entry
parseEntry str =
    let tokens = splitOn " " str
        bounds = map read $ splitOn "-" $ head tokens :: [Int]
    in Entry { minOccurs = bounds !! 0
             , maxOccurs = bounds !! 1
             , char = head $ tokens !! 1
             , password = tokens !! 2
             }

isValidPassword :: Entry -> Bool
isValidPassword (Entry { minOccurs = lower
                       , maxOccurs = upper
                       , char = c
                       , password = pass}) =
   let occurences = length $ filter ((==) c) pass
    in lower <= occurences && occurences <= upper

countValidPasswords :: [Entry] -> Int
countValidPasswords es = length $ filter (isValidPassword) es

isNewRulePassword :: Entry -> Bool
isNewRulePassword (Entry { minOccurs = lower
                         , maxOccurs = upper
                         , char = c
                         , password = pass}) =
     xor ((pass !! (lower - 1)) == c) 
         ((pass !! (upper - 1)) == c)

countNewRulePasswords :: [Entry] -> Int
countNewRulePasswords es = length $ filter (isNewRulePassword) es

main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let parsedData  = map parseEntry $ lines contents
    putStrLn $ show $ countValidPasswords parsedData
    putStrLn $ show $ countNewRulePasswords parsedData
    hClose handle
