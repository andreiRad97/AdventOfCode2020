import qualified Data.IntMap.Lazy as IntMap
import Data.List.Split
import System.IO

type PosMap = IntMap.IntMap Int

consumeInput :: [Int] -> PosMap
consumeInput xs = IntMap.fromList $ zip xs [1..length xs] 

generate :: Int -> PosMap -> Int -> Int -> Int
generate lastNumber numberToLastTurn currentTurn stop =
    if currentTurn == stop + 1 then lastNumber else
    let nextNumber = 
            case IntMap.member lastNumber numberToLastTurn of
                False -> 0
                True -> currentTurn - 1 - (numberToLastTurn IntMap.! lastNumber) 
        numberToLastTurn' = IntMap.insert lastNumber (currentTurn - 1) numberToLastTurn
     in generate nextNumber numberToLastTurn' (currentTurn + 1) stop

solve :: String -> Int -> Int
solve input target = 
    let numbers = map read $ splitOn "," input :: [Int]
        initialMap = consumeInput (init numbers)
     in generate (last numbers) initialMap (length numbers + 1) target

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solve contents 2020
    putStrLn $ show $ solve contents 30000000 -- Only works when compiled with -O2 flag
    hClose handle
