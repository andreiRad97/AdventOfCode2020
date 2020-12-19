import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

type Coord = (Int, Int, Int, Int)
type Config = Map.Map Coord Bool

-- Parsing

parseCell :: Config -> (Int, Int) -> Char -> (Config, Int)
parseCell config (row, col) c = (Map.insert (row, col, 1, 1) (c == '#') config, col + 1)

parseRow :: (Config, Int) -> String -> (Config, Int)
parseRow (config, row) rowStr = 
    let nextCol (conf', col) cell = parseCell conf' (row, col) cell
     in (fst $ foldl nextCol (config, 1) rowStr, row + 1)
   

parse :: String -> Config
parse input = fst $ foldl parseRow (Map.empty, 1) (lines input)

-- Cycle simulation

-- includes the initial Coord as well
getAdj :: Coord -> [Coord]
getAdj (x, y, z, w) = 
    [ (x + i, y + j, z + k, w + l) | i <- [-1..1], j <- [-1..1], k <- [-1..1], l <- [-1..1]]

getState :: Config -> Coord -> Bool
getState config coord =
    case Map.lookup coord config of
      Just True -> True
      _ -> False

getNextState :: Coord -> Config -> Bool
getNextState coord config = 
    let activeNeighbours = length $ filter id $ map (getState config) $ getAdj coord in
    case getState config coord of
      True -> (activeNeighbours == 3) || (activeNeighbours == 4) 
      False -> activeNeighbours == 3

getNextConfig :: Config -> Config
getNextConfig conf =
    let allCells = 
            foldl 
                (\computed coord -> Set.union computed (Set.fromList $ getAdj coord)) 
                Set.empty 
                (Map.keys conf)
     in foldl 
            (\currConf coord -> Map.insert coord (getNextState coord conf) currConf)
            Map.empty
            allCells


solve :: String -> Int -> Int
solve input targetConfig =
    let initConfig = parse input
        finalConfig = head $ drop targetConfig $ iterate getNextConfig initConfig
     in length $ filter id $ Map.elems finalConfig

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solve contents 6
    hClose handle
