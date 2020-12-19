import Data.List.Split
import Data.Sort
import System.IO

type Restriction = (String, (Int,Int), (Int, Int))
type Ticket = [Int]

parseSingleRestr :: String -> Restriction
parseSingleRestr str =
    let tokens = splitOn ":" str
        name = head tokens
        dashedNums = splitOn " or " $ last tokens
        nums = map read $ concat $ map (splitOn "-") dashedNums 
     in (name, (nums !! 0, nums !! 1), (nums !! 2, nums !! 3))

parseRestrs :: String -> [Restriction]
parseRestrs str = map parseSingleRestr $ lines str

parseSingleTicket :: String -> Ticket
parseSingleTicket t = map read $ splitOn "," t

parseTickets :: String -> [Ticket]
parseTickets ts = map parseSingleTicket $ tail $ lines ts

parse :: String -> ([Restriction], Ticket, [Ticket])
parse input =
    let splits = splitOn "\n\n" input 
    in (parseRestrs $ splits !! 0
       ,head $ parseTickets $ splits !! 1
       ,parseTickets $ splits !! 2
       )

respectsRestriction :: Int -> Restriction -> Bool
respectsRestriction x (_, (a, b), (c, d)) = (a <= x && x <= b) || (c <= x && x <= d)

isValidField :: Int -> [Restriction] -> Bool
isValidField x rs = foldl (\a b -> a || respectsRestriction x b) False rs

sumMistakes :: Ticket -> [Restriction] -> Int
sumMistakes ticket restrictions = sum $ filter (\x -> not $ isValidField x restrictions) ticket

solveFirst :: String ->Int
solveFirst input =
    let (restrictions, ownTicket, nearbyTickets) = parse input
     in sum $ map (flip sumMistakes restrictions) nearbyTickets

-- Part 2

type RestrictionIndex = (String, Int)

startsWithDeparture :: String -> Bool
startsWithDeparture name = "departure" == (head $ splitOn " " name)

isValidTicket :: Ticket -> [Restriction] -> Bool
isValidTicket t restrictions = all (flip isValidField restrictions) t

transpose :: [Ticket] -> [Ticket]
transpose xs =
    if all ((==) []) xs then [] 
    else (map head xs) : (transpose $ map tail xs)

findRow :: [([Int], Int)] -> Restriction -> RestrictionIndex
findRow [] (name, _, _) = error $ "Could not find row for restriction " ++ name
findRow ((x, i):xs) (name, firstRule, secondRule) =
    if isValidTicket x [(name, firstRule, secondRule)] then (name, i)
    else  findRow xs (name, firstRule, secondRule)

findRows colIndices [] = []
findRows colIndices (r:rs) =
    let (name, i) = findRow colIndices r
        remainingCols = filter (\x -> i /= snd x) colIndices
     in (name, i) : (findRows remainingCols rs)

getRestrictionsIndices :: [Restriction] -> [Ticket] -> [RestrictionIndex]
getRestrictionsIndices restrictions tickets =
    let cols = transpose tickets   
        colIndices = zip cols [0..length cols - 1]
     in findRows colIndices restrictions


sortByValidFields :: [Ticket] -> [Restriction] -> [Restriction]
sortByValidFields tickets restrictions =
    let fields = transpose tickets
        countValidFields r = length $ filter ((==) True) $ map (flip isValidTicket [r]) fields
        fieldCount = map (\r -> (countValidFields r, r)) restrictions
     in map snd $ sort fieldCount


solveSecond :: String -> Int
solveSecond input =
    let (restrictions, ownTicket, nearbyTickets) = parse input
        validTickets = ownTicket : filter (flip isValidTicket restrictions) nearbyTickets
        sortedRestrictions = sortByValidFields validTickets restrictions
        restrictionIndices = getRestrictionsIndices sortedRestrictions validTickets
     in product $ map (\x -> ownTicket !! x) $ map snd $ filter (startsWithDeparture . fst) restrictionIndices
     

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents
    putStrLn $ show $ solveSecond contents
    hClose handle
