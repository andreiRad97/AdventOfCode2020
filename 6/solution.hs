import Data.Set (empty, fromList, intersection, size, union)

import System.IO

accumulateLines :: [[String]] -> [String] -> [String] -> [[String]]
accumulateLines prevLines [] [] = prevLines
accumulateLines prevLines currLine [] = currLine : prevLines
accumulateLines prevLines currLine (x:xs) =
    case x of
      "" -> accumulateLines (currLine : prevLines) [] xs
      _ -> accumulateLines prevLines (x : currLine) xs

separateGroups :: String -> [[String]]
separateGroups input =
    let separatedLines = lines input
     in accumulateLines [] [] separatedLines

countGroupQuestions :: [String] -> Int
countGroupQuestions groupAnswers =
    size $ foldl union empty $ map fromList groupAnswers

countGroupCommonAnswers :: [String] -> Int
countGroupCommonAnswers groupAnswers =
    let answers = map fromList groupAnswers
     in size $ foldl intersection (head answers) (tail answers)

sumQuestions :: String -> Int
sumQuestions input =
    let groupAnswers = separateGroups input
        groupTotals = map countGroupQuestions groupAnswers
     in sum groupTotals

sumCommonQuestions :: String -> Int
sumCommonQuestions input =
    let groupAnswers = separateGroups input
        groupTotals = map countGroupCommonAnswers groupAnswers
     in sum groupTotals

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ sumQuestions $ contents
    putStrLn $ show $ sumCommonQuestions $ contents
    hClose handle
