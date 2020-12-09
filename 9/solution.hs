import qualified Data.Map as Map
import Data.List

import System.IO

data SlowQueue a = SlowQueue [a]

push :: SlowQueue a -> a -> SlowQueue a
push (SlowQueue xs) x = SlowQueue (xs ++ [x])

pop :: SlowQueue a -> (a, SlowQueue a)
pop (SlowQueue []) = error "Pop from empty queue"
pop (SlowQueue (x:xs)) = (x, SlowQueue xs)

contents :: SlowQueue a -> [a]
contents (SlowQueue xs) = xs

size :: SlowQueue a -> Int
size (SlowQueue xs) = length xs

toList :: SlowQueue a -> [a]
toList (SlowQueue xs) = xs

type CountMap = Map.Map Int Int

decrement :: CountMap -> Int -> CountMap
decrement numMap x =
    case Map.lookup x numMap of
      Nothing -> error $ "Cannot decrement " ++ (show x)
      Just 0 -> error $ "Cannot decrement " ++ (show x)
      Just 1 -> Map.delete x numMap
      Just y -> Map.insert x (y - 1) numMap

increment :: CountMap -> Int -> CountMap
increment numMap x =
    case Map.lookup x numMap of
      Nothing -> Map.insert x 1 numMap
      Just y -> Map.insert x (y + 1) numMap

findSumPair :: CountMap -> Int -> Maybe (Int, Int)
findSumPair numToCount x =
    let numList = map fst $ Map.toList numToCount
    in case find (\a -> Map.member (x-a) (decrement numToCount a)) numList of
        Nothing -> Nothing
        Just y -> Just (y, x - y)

findInvalidNumber :: SlowQueue Int -> CountMap -> [Int] -> Int
findInvalidNumber _ _ [] = error "Could not find invalid number"
findInvalidNumber slidingWindow numToCount (x:xs) = 
    case findSumPair numToCount x of
      Nothing -> x
      _ -> 
          let (eliminated, tempWindow) = pop slidingWindow
              slidingWindow' = push tempWindow x
              numToCount' = increment (decrement numToCount eliminated) x
           in findInvalidNumber slidingWindow' numToCount' xs

solveFirst :: String -> Int
solveFirst input =
    let nums = map read $ lines input
        initNums = take 25 nums
        slidingWindow = SlowQueue initNums
        initMap = Map.fromList $ zip initNums [1 | x <- [1..25]]
     in findInvalidNumber slidingWindow initMap (drop 25 nums)

popExtra :: Int -> Int -> SlowQueue Int -> (Int, SlowQueue Int)
popExtra sum target queue =
    case sum <= target of
      True -> (sum, queue)
      False -> let (x, queue') = pop queue in popExtra (sum - x) target queue'

findSum :: Int -> Int -> SlowQueue Int -> [Int] -> Int
findSum _ _ _  [] = error "Could not find sequence"
findSum invalid currSum queue (x:xs) =
    let currSum' = currSum + x
        queue' = push queue x
        (newSum, newQueue) = popExtra currSum' invalid queue'
     in case newSum == invalid && (size newQueue) > 1 of
          True -> let lst = toList newQueue in (maximum lst) + (minimum lst)
          False -> findSum invalid newSum newQueue xs

findSumOfSequence :: Int -> [Int] -> Int
findSumOfSequence invalid nums =
    findSum invalid 0 (SlowQueue []) nums

solveSecond :: String -> Int
solveSecond input = 
    let invalid = solveFirst input -- wasteful
        nums = map read $ lines input
     in findSumOfSequence invalid nums

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents
    putStrLn $ show $ solveSecond contents
    hClose handle
