import qualified Data.Map as Map
import System.IO

data Cell = Empty | Occupied | Floor deriving (Eq, Show)

type Position = (Int, Int)

type Grid = Map.Map Position Cell

-- Grid 
equal :: Grid -> Grid -> Bool
equal g1 g2 = (Map.isSubmapOf g1 g2) && (Map.isSubmapOf g2 g1)

countOccupiedSeats :: Grid -> Int
countOccupiedSeats grid = Map.size $ Map.filter ((==) Occupied) grid 

directions :: [Position]
directions = [(i,j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0)]

getAdjOccupied :: Grid -> Position -> [Position]
getAdjOccupied grid (x,y) =
    let adjPositions = map (\(i,j) -> (x + i, y + j)) directions
        adjCellStates = map (\x -> (x, Map.lookup x grid)) adjPositions
     in map fst $ filter (\x -> (snd x) == (Just Occupied)) adjCellStates

getNextCellState :: Grid -> Position -> Cell -> Cell
getNextCellState grid pos cellState =
    let neighbours = length $ getAdjOccupied grid pos
     in case cellState of
          Floor -> Floor
          Occupied -> if 4 <= neighbours then Empty else Occupied
          Empty -> if 0 == neighbours then Occupied else Empty

next :: Grid -> Grid
next grid = Map.mapWithKey (getNextCellState grid) grid

getFinal :: (Grid -> Grid) -> Grid -> Grid
getFinal iterFunc grid =
    let nextGrid = iterFunc grid
     in case equal grid nextGrid of
          True -> grid
          False -> getFinal iterFunc nextGrid

-- Part 2

findFirstSeat :: Grid -> Position -> Position -> Maybe Cell
findFirstSeat grid (x,y) (dx,dy) =
    let nextPos = (x + dx, y + dy)
    in case Map.lookup nextPos grid of
         Just Floor -> findFirstSeat grid nextPos (dx,dy)
         seat -> seat
      

getNextCellState2 :: Grid -> Position -> Cell -> Cell
getNextCellState2 grid pos cell =
    let seenSeats = map (findFirstSeat grid pos) directions
        occupiedSeats = length $ filter ((==) (Just Occupied)) seenSeats
     in case cell of
          Empty -> if 0 == occupiedSeats then Occupied else Empty
          Occupied -> if 5 <= occupiedSeats then Empty else Occupied
          Floor -> Floor

next2 :: Grid -> Grid
next2 grid = Map.mapWithKey (getNextCellState2 grid) grid


-- Parsing
getCell :: Char -> Cell
getCell 'L' = Empty
getCell '#' = Occupied
getCell '.' = Floor
getCell x = error $ "Unrecognized token: " ++ [x]

parseLine :: String -> [Cell]
parseLine "" = []
parseLine (x:xs) = (getCell x) : (parseLine xs)

buildGrid :: [[Cell]] -> Grid
buildGrid cells =
    let indexedRows = zip [1..length cells] cells
        indexCols = \(rowIndex, row) -> zip [(rowIndex, i) | i <- [1..length row]] row
     in Map.fromList $ concat $ map indexCols indexedRows

-- Final functions
solveFirst :: String -> Int
solveFirst input =
    let grid = buildGrid $ map parseLine $ lines input
     in countOccupiedSeats $ getFinal next grid

solveSecond :: String -> Int
solveSecond input =
    let grid = buildGrid $ map parseLine $ lines input
     in countOccupiedSeats $ getFinal next2 grid

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents 
    putStrLn $ show $ solveSecond contents 
    hClose handle
