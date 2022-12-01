module Utils where

import Text.Printf (printf)

printPartOneAndTwo :: Integer -> Integer -> IO ()
printPartOneAndTwo part1 part2 = do
  printf "%s: %d \n" "Part one" part1
  printf "%s: %d \n" "Part two" part2

readFileAsList :: FilePath -> IO [String]
readFileAsList fileName = do
  contents <- readFile fileName
  let lns = lines contents
  return lns

splitListAt :: String -> [String] -> [[String]]
splitListAt _ [] = [[]]
splitListAt s list =
  let l = last list
      current = splitListAt s (init list)
   in if l == s then current ++ [[]] else init current ++ [last current ++ [l]]
