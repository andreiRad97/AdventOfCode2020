import Data.List.Split
import Math.NumberTheory.Moduli.Chinese
import System.IO
import Text.Read

parseSchedule :: [String] -> [Int]
parseSchedule [] = []
parseSchedule (x:xs) =
    case readMaybe x :: Maybe Int of
      Just y -> y : (parseSchedule xs)
      Nothing -> parseSchedule xs

parseInput :: String -> (Int, [Int])
parseInput input =
    let inputLines = lines input 
        scheduleString = splitOn "," $ inputLines !! 1
     in (read $ head inputLines :: Int, parseSchedule scheduleString)


solveFirst :: String -> Int
solveFirst input =
    let (timestamp, ids) = parseInput input
        waitingTimes = map (\x -> (x - timestamp `mod` x, x)) ids
        (time, id) = minimum waitingTimes
     in time * id

-- Second part

parseSchedule2 :: [String] -> Int -> [(Integer, Integer)]
parseSchedule2 [] _ = []
parseSchedule2 (x:xs) index =
    case readMaybe x :: Maybe Integer of
      Just y -> 
        let remainder = (y - (toInteger index)) `mod` y 
        in (remainder, y) : parseSchedule2 xs (index + 1)
      Nothing -> parseSchedule2 xs (index + 1)

parseInput2 :: String -> [(Integer, Integer)]
parseInput2 input = 
    let inputLines = splitOn "," $ (lines input) !! 1 
     in parseSchedule2 inputLines 0

solveSecond :: String -> Integer
solveSecond input =
    let timesIndices = parseInput2 input
     in case chineseRemainder timesIndices of
          Just x -> x
          _ -> error "Could not compute number for second task"

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents 
    putStrLn $ show $ solveSecond contents 
    hClose handle
