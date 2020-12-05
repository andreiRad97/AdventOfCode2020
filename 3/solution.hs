import System.IO

data Slope = Slope { right :: Int, down :: Int }

countTrees':: Slope -> [String] -> Int -> Int -> Int
countTrees' _ [] acc _ = acc
countTrees' (Slope { right = r, down = d }) (x:xs) acc pos =
    let square = x !! pos
        newAcc = case square of
                   '#' -> acc + 1
                   _   -> acc
        nextPos = (pos + r) `mod` (length x)
    in countTrees' 
        (Slope { right = r, down = d })
        (drop (d - 1) xs)
        newAcc
        nextPos

slopes = [ Slope {right = 1, down = 1}
         , Slope {right = 3, down = 1}
         , Slope {right = 5, down = 1}
         , Slope {right = 7, down = 1}
         , Slope {right = 1, down = 2}
         ]

countTrees :: [String] -> Int
countTrees xs = countTrees' (slopes !! 1) xs 0 0

countAllTrees :: [Slope] -> [String] -> Int
countAllTrees ss xs =
    let multiply x slope = x * (countTrees' slope xs 0 0)
    in foldl multiply 1 ss
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let squares = lines contents
    let firstRes = countTrees squares
    putStrLn $ show $ firstRes
    let secondRes = countAllTrees slopes squares
    putStrLn $ show $ secondRes
    hClose handle
