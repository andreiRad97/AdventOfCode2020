import System.IO

type Position = (Int, Int)
type Orientation = Int
type BoatState = (Position, Orientation)

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

data Instr = North Int 
           | South Int
           | East Int
           | West Int
           | Left Int
           | Right Int
           | Forward Int

parseInstr :: String -> Instr
parseInstr str =
     let nr = read $ tail str :: Int
      in case head str of
           'N' -> North nr
           'S' -> South nr
           'E' -> East nr
           'W' -> West nr
           'L' -> Main.Left nr
           'R' -> Main.Right nr
           'F' -> Forward nr

parseInput :: String -> [Instr]
parseInput input = map parseInstr $ lines input


moveBoat :: BoatState -> Instr -> BoatState
moveBoat ((x, y), o) (North val) = ((x, y + val), o)
moveBoat ((x, y), o) (South val) = ((x, y - val), o)
moveBoat ((x, y), o) (East val) = ((x + val, y), o)
moveBoat ((x, y), o) (West val) = ((x - val, y), o)
moveBoat ((x, y), o) (Main.Left val) = ((x,y), (o + val) `mod` 360)
moveBoat ((x, y), o) (Main.Right val) = moveBoat ((x,y), o) (Main.Left (360 - val))
moveBoat ((x, y), o) (Forward val) =
    case o of
      0 -> ((x + val, y), o)
      90 -> ((x, y + val), o)
      180 -> ((x - val, y), o)
      270 -> ((x, y - val), o)
      _ -> error $ "Cannot use orientation " ++ (show o)

solveFirst :: [Instr] -> Int
solveFirst instrs = 
    let initialState = ((0,0), 0)
        finalState = foldl moveBoat initialState instrs
     in manhattanDistance (fst finalState) (fst initialState)

rotateLeft :: Position -> Int -> Position
rotateLeft (x, y) 0 = (x, y)
rotateLeft (x, y) 90 = (-y, x)
rotateLeft (x, y) 180 = (-x, -y)
rotateLeft (x, y) 270 = (y, -x)
rotateLeft _ v = error $ "Cannot rotate by " ++ (show v)

-- Second Part

type NavigationState = (Position, Position) 

move :: NavigationState -> Instr -> NavigationState
move (boatPos, (x,y)) (North val) = (boatPos, (x, y + val))
move (boatPos, (x,y)) (South val) = (boatPos, (x, y - val))
move (boatPos, (x,y)) (East val) = (boatPos, (x + val, y))
move (boatPos, (x,y)) (West val) = (boatPos, (x - val, y))
move (boatPos, wayPointPos) (Main.Left val) = 
    (boatPos, rotateLeft wayPointPos val)
move (boatPos, wayPointPos) (Main.Right val) =
    (boatPos, rotateLeft wayPointPos (360 -val))
move ((bx, by), (wx, wy)) (Forward val) = 
    (((bx + val * wx), (by + val * wy)), (wx, wy))

solveSecond :: [Instr] -> Int
solveSecond instrs = 
    let initialState = ((0,0), (10, 1))
        finalState = foldl move initialState instrs
     in manhattanDistance (fst finalState) (fst initialState)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst $ parseInput contents
    putStrLn $ show $ solveSecond $ parseInput contents
    hClose handle
