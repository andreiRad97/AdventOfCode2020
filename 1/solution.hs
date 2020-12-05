import System.IO
import Data.List.Split
import Data.List

findPair value _ [] = Nothing
findPair value [] _ = Nothing
findPair value (x:xs) (y:ys) = 
    case compare (x+y) value of
      EQ -> Just (x, y)
      LT -> findPair value xs (y:ys)
      GT -> findPair value (x:xs) ys

getPair value xs = 
    let (firstHalf, secondHalf) = partition ((>=) (value `div` 2)) xs
     in findPair value firstHalf $ reverse secondHalf

getPairProduct value xs =
    case getPair value (sort xs) of
         Nothing -> error "Could not find pair"
         Just (x, y) -> x * y

getTripleProduct value [] = error "Could not find triple"
getTripleProduct value (x:xs) =
    case getPair (value - x) (sort xs) of
      Just (y, z) -> x * y * z
      Nothing -> getTripleProduct value xs

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let inputValues = map read $ words contents :: [Int]
    putStrLn $ show $ getPairProduct 2020 inputValues
    putStrLn $ show $ getTripleProduct 2020 inputValues
    hClose handle
