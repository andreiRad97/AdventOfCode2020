import Data.Char (isDigit)
import System.IO

data Expr = Plus Expr Expr | Times Expr Expr | Num Int deriving Show
data Token = PlusT | TimesT | NumT Int | LPar | RPar deriving (Show, Eq)

handleBuffer :: String -> ([Token] -> [Token])
handleBuffer "" = id
handleBuffer buffer = \ts -> (NumT $ read $ reverse buffer) : ts

charToToken :: Char -> Token
charToToken '+' = PlusT
charToToken '*' = TimesT
charToToken '(' = LPar
charToToken ')' = RPar

tokenize :: String -> String -> [Token]
tokenize buffer "" = handleBuffer buffer []
tokenize buffer (' ':cs) = tokenize buffer cs
tokenize buffer (c:cs) =
    if isDigit c then tokenize (c:buffer) cs
    else handleBuffer buffer $ charToToken c : (tokenize "" cs)

buildExpr :: [Token] -> [Expr] -> [Expr]
buildExpr [] es = es
buildExpr (t:ts) (e1:e2:es) =
    case t of
        PlusT -> buildExpr ts $ (Plus e1 e2) : es
        TimesT -> buildExpr ts $ (Times e1 e2) : es

prec1 :: Token -> Int
prec1 TimesT = 1
prec1 PlusT = 1
prec1 _ = 0

prec2 :: Token -> Int
prec2 TimesT = 1
prec2 PlusT = 2
prec2 _ = 0

parse :: (Token -> Int) -> [Token] -> [Expr] -> [Token] -> Expr
parse prec opStack exprStack [] = head $ buildExpr opStack (exprStack)
parse prec opStack exprStack ((NumT n) : ts) = parse prec opStack ((Num n) : exprStack) ts
parse prec opStack exprStack (LPar : ts) = parse prec (LPar : opStack) exprStack ts
parse prec opStack exprStack (RPar : ts) = 
    let neededTokens = takeWhile ((/=) LPar) opStack
        newOpStack = tail $ dropWhile ((/=) LPar) opStack
        newExprStack = buildExpr neededTokens (exprStack)
     in parse prec newOpStack newExprStack ts
parse prec opStack exprStack (t:ts) =
    let neededTokens = takeWhile (\x -> prec t <= prec x) opStack
        newOpStack = t : (dropWhile (\x -> prec t <= prec x) opStack)
        newExprStack = buildExpr neededTokens (exprStack)
     in parse prec newOpStack newExprStack ts

eval :: Expr -> Int
eval (Num n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2


calculate :: (Token -> Int) -> String -> Int
calculate prec input = eval $ parse prec [] [] $ tokenize "" input

solveFirst :: String -> Int
solveFirst input = sum $ map (calculate prec1) (lines input)

solveSecond :: String -> Int
solveSecond input = sum $ map (calculate prec2) (lines input)

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solveFirst contents
    putStrLn $ show $ solveSecond contents
    hClose handle
