module Utils where

import Text.Printf (PrintfArg, printf)

printPartOneAndTwo :: (PrintfArg a) => a -> a -> IO ()
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

splitListInHalf :: [a] -> ([a], [a])
splitListInHalf list = let half = length list `div` 2 in splitAt half list

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs in ys : chunks n zs