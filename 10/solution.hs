import System.IO
import Data.List

count :: Eq a => a -> [a] -> Int 
count x xs = length $ filter ((==) x) xs


getAllVoltages :: [Int] -> [Int]
getAllVoltages voltages =
    let outletVoltage = 0
        deviceVoltage = (maximum voltages) + 3
     in (sort $ outletVoltage : voltages) ++ [deviceVoltage]

solveFirst :: [Int] -> Int
solveFirst voltages =
    let allVoltages = getAllVoltages voltages
        diffs =  map (\(x,y) -> y - x) $ zip (init allVoltages) (tail allVoltages)
     in (count 1 diffs) * (count 3 diffs)

countSetsByHead :: [Int] -> [(Int,Int)]
countSetsByHead [] = []
countSetsByHead [x] = [(1,x)]
countSetsByHead (x:xs) =
    let nextSets = countSetsByHead xs
        compatibleAdapters = takeWhile (\y -> (snd y) - x <= 3) nextSets
     in (sum $ map fst compatibleAdapters, x) : nextSets

solveSecond :: [Int] -> Int
solveSecond = fst.head.countSetsByHead.getAllVoltages


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = map read $ lines contents
    putStrLn $ show $ solveFirst input 
    putStrLn $ show $ solveSecond input 
    hClose handle
