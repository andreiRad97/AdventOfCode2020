import qualified Data.Map as Env

import Control.Exception.Assert
import Data.Bits
import Data.Char
import Data.List.Split
import System.IO

data Op = SetMask String Integer Integer | SetMem Integer Integer deriving Show

type Memory = Env.Map Integer Integer
type Mask = (String, Integer, Integer)

applyMask :: Mask -> Integer -> Integer
applyMask (_, zeroes, ones) x = (x .&. zeroes) .|. ones

runProgram :: Memory -> Mask -> [Op] -> Memory
runProgram memory _ [] = memory
runProgram memory _ ((SetMask _ zeroes ones):xs) = runProgram memory ([], zeroes, ones) xs
runProgram memory mask ((SetMem x y):xs) =
    let memory' = Env.insert x (applyMask mask y) memory
     in runProgram memory' mask xs

parseSetMem :: String -> String -> Op
parseSetMem lhs rhs =
    let location = read $ init $ drop 4 lhs :: Integer
        value = read rhs :: Integer
     in SetMem location value

parseSetMask' :: Integer -> Integer -> String -> (Integer, Integer)
parseSetMask' zeroes ones [] = (zeroes, ones)
parseSetMask' zeroes ones (x:xs) = 
    case x of
      '0' -> parseSetMask' (zeroes * 2) (ones * 2) xs
      '1' -> parseSetMask' (zeroes * 2 + 1) (ones * 2 + 1) xs
      'X' -> parseSetMask' (zeroes * 2 + 1) (ones * 2) xs

parseSetMask :: String -> Op
parseSetMask mask = 
    let (zeroes, ones) = parseSetMask' 0 0 mask
        maskList = generateMaskList $ reverse mask
     in SetMask mask zeroes ones

parseOp :: String -> Op
parseOp op =
    let tokens = splitOn " = " op
     in case head tokens of
          "mask" -> parseSetMask (tokens !! 1)
          _ -> parseSetMem (tokens !! 0) (tokens !! 1)

parseInput :: String -> [Op]
parseInput input = map parseOp $ lines input

solveFirst :: String -> Integer
solveFirst input = Env.foldl (+) 0 $ runProgram Env.empty ([], 0,0) $ parseInput input

-- Input mask must be reversed, output is also reversed!
genString :: String -> Integer -> String
genString "X" num = "X"
genString "0" num = show num
genString "1" num = "1"
genString ('X':xs) num = 'X' : (genString xs (num `div` 2))
genString ('0':xs) num = (head $ show $ num `mod` 2) : (genString xs (num `div` 2))
genString ('1':xs) num = '1' : (genString xs (num `div` 2))

-- Mask must be reversed!
generateMaskList :: String -> [Integer]
generateMaskList "X" = [0, 1]
generateMaskList "0" = [0]
generateMaskList "1" = [1]
generateMaskList ('X':xs) = 
    let maskTail = generateMaskList xs
     in (map (\x -> x * 2) maskTail) ++ (map (\x -> x * 2 + 1) maskTail)
generateMaskList (x:xs) =
    let value = read [x] :: Integer
     in map (\x -> x * 2 + value) $ generateMaskList xs

generateAddrs :: Mask -> Integer -> [Integer]
generateAddrs (mask, _, _) x = generateMaskList $ genString (reverse mask) x

runProgram2 :: Memory -> Mask -> [Op] -> Memory
runProgram2 memory _ [] = memory
runProgram2 memory _ ((SetMask mask _ _):xs) = runProgram2 memory (mask, 0, 0) xs
runProgram2 memory mask ((SetMem x y):xs) =
    let memory' = foldl (\mem addr -> Env.insert addr y mem) memory $ generateAddrs mask x
     in runProgram2 memory' mask xs

solveSecond :: String -> Integer
solveSecond input = Env.foldl (+) 0 $ runProgram2 Env.empty ([], 0,0) $ parseInput input

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents 
    putStrLn $ show $ solveSecond contents 
    hClose handle
