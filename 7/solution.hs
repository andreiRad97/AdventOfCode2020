import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List.Split

import System.IO

type StringSet = Set.Set String
type BagGraph = Map.Map String StringSet

dfs :: BagGraph -> String -> StringSet -> StringSet
dfs graph bag visited =
    if Set.member bag visited then visited else
    let visited' = Set.insert bag visited
     in case Map.lookup bag graph of
          Nothing -> visited'
          Just adjacents -> 
              Set.foldl' Set.union visited' $ 
                  Set.map (\x -> dfs graph x visited') adjacents

countBag :: BagGraph -> String -> Int
countBag graph bag = (Set.size $ dfs graph bag Set.empty) - 1


type BagCount = (Int, String)
type BagContents = (String, [BagCount])

parseBag :: String -> BagCount
parseBag str =
    let tokens = splitOn " " str in
        (read $ head tokens :: Int, (tokens !! 1) ++ " " ++ (tokens !! 2))

parseInnerBags :: String -> [BagCount]
parseInnerBags str =
    case str of
      "no other bags." -> []
      _ -> map parseBag $ splitOn ", " $ init str

parseLine :: String -> BagContents
parseLine line = 
    let tokens = splitOn " contain " line
        color = head $ splitOn " bags" $ head tokens
     in (color, parseInnerBags $ last tokens) 


insertBag :: String -> BagGraph -> BagCount -> BagGraph
insertBag bag graph (_, innerBag) =
    case Map.lookup innerBag graph of
      Nothing -> Map.insert innerBag (Set.singleton bag) graph
      Just bs -> Map.insert innerBag (Set.insert bag bs) graph

insertBags :: BagGraph -> BagContents -> BagGraph
insertBags graph (bag, contents) =
    foldl (insertBag bag) graph contents

createGraph :: String -> BagGraph
createGraph input =
    let bags = map parseLine $ lines input
     in foldl insertBags Map.empty bags

type BagCountSet = Set.Set BagCount
type BagCountGraph = Map.Map String BagCountSet

insertTree :: BagCountGraph -> BagContents -> BagCountGraph
insertTree tree (bag, bagCount) = Map.insert bag (Set.fromList bagCount) tree

createTree :: String -> BagCountGraph
createTree input =
    let bags = map parseLine $ lines input
     in foldl insertTree Map.empty bags

countContainedBags :: BagCountGraph -> String -> Int
countContainedBags graph bag =
    case Map.lookup bag graph of
      Nothing -> 0
      Just xs -> 
          foldl (\x (y,z) -> x + y * (1 +  (countContainedBags graph z))) 0 xs

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ countBag (createGraph contents) "shiny gold" 
    putStrLn $ show $ countContainedBags (createTree contents) "shiny gold" 
    hClose handle
