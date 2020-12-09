import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List.Split

import System.IO

data Op = Nop Int | Jmp Int | Acc Int deriving Show

data State = State { accum :: Int
                   , currInstr :: Int
                   , executedInstrs :: Set.Set Int }

constructState acc ci ei = State { accum = acc
                                 , currInstr = ci
                                 , executedInstrs = ei
                                 }

type Program = Map.Map Int Op

parseInt :: String -> Int
parseInt str =
    case head str of
      '+' -> read $ tail str
      '-' -> read str
      _ -> error $ "Unrecognised integer: " ++ str

parseOp :: String -> Op
parseOp line =
    let tokens = splitOn " " line
        intValue = parseInt $ (head . tail) tokens
     in case head tokens of
          "nop" -> Nop intValue
          "acc" -> Acc intValue
          "jmp" -> Jmp intValue
          _ -> error $ "Unrecognised instruction: " ++ line

parseCode :: String -> [Op]
parseCode input = map parseOp $ lines input

createProgram :: [Op] -> Program
createProgram instrs = Map.fromList $ zip [1..length instrs] instrs

executeOp :: Op -> Int -> Int -> (Int, Int)
executeOp (Nop _) accum currInstr = (accum, currInstr + 1)
executeOp (Jmp x) accum currInstr = (accum, currInstr + x)
executeOp (Acc x) accum currInstr = (accum + x, currInstr + 1)

runProgram :: Program -> State -> (Int, Bool)
runProgram prog (State { accum = acc
                       , currInstr = ci
                       , executedInstrs = ei 
                       }) =
    let infiniteLoop = Set.member ci ei 
        programFinished = infiniteLoop || ci == 1 + (Map.size prog)
     in case programFinished of 
          True -> (acc, infiniteLoop)
          False ->
              let ei' = Set.insert ci ei
                  currOp = prog Map.! ci
                  (acc', ci') = executeOp currOp acc ci 
               in runProgram prog $ constructState acc' ci' ei'

initialState = State { accum = 0
                     , currInstr = 1
                     , executedInstrs = Set.empty
                     }
    
solveFirst :: String -> Int
solveFirst input = 
     fst $ runProgram (createProgram $ parseCode input) initialState

flipOp :: Op -> Op
flipOp (Acc x) = Acc x
flipOp (Jmp x) = Nop x
flipOp (Nop x) = Jmp x


flipProgram :: Int -> Program -> [Program] -> [Program]
flipProgram index prog accum =
    let op = prog Map.! index 
    in case op of
         Acc x -> accum
         _ -> (Map.insert index (flipOp op) prog) : accum

generateFlippedPrograms :: Program -> [Program]
generateFlippedPrograms prog = 
    foldl (\lst i -> flipProgram i prog lst) [] (Map.keys prog)


solveSecond :: String -> Int
solveSecond input = 
    let prog = createProgram $ parseCode input
        flippedPrograms = generateFlippedPrograms prog
        results = map (\p -> runProgram p initialState) flippedPrograms
     in fst $ head $ filter (\x -> not $ snd x) results


main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents
    putStrLn $ show $ solveSecond contents
    hClose handle
