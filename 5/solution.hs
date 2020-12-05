import Control.Exception
import Data.List
import System.IO

data Move = Advance | Retreat

getMovementType :: Char -> Move
getMovementType 'R' = Advance
getMovementType 'B' = Advance
getMovementType 'F' = Retreat
getMovementType 'L' = Retreat
getMovementType c = error $ "Unrecognized movement token " ++ [c]

calculateBin :: String -> Int -> Int -> Int
calculateBin "" lowerBound upperBound = assert (lowerBound == (upperBound - 1)) lowerBound
calculateBin (x:xs) lowerBound upperBound =
    let mid = (lowerBound + upperBound) `div` 2
     in case getMovementType x of
          Advance -> calculateBin xs mid upperBound
          Retreat -> calculateBin xs lowerBound mid

strToId :: String -> Int
strToId xs =
    let row = calculateBin (take 7 xs) 0 128
        col = calculateBin (drop 7 xs) 0 8
     in row * 8 + col


getMaxId :: [String] -> Int
getMaxId xs = maximum $ map strToId xs

getMissingNumber :: [Int] -> Int
getMissingNumber (x:(y:xs)) =
    case y - x of
      2 -> x + 1
      _ -> getMissingNumber (y:xs)
getMissingNumber _ = error "Could not find missing ticket id"

getOwnId :: [String] -> Int
getOwnId xs =
    let ids = sort $ map strToId xs
     in getMissingNumber ids

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ getMaxId $ lines contents
    putStrLn $ show $ getOwnId $ lines contents
    hClose handle
