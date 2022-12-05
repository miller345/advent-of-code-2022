module Utils where

import Text.Printf (printf)

printPartOneAndTwo :: (Show a) => a -> a -> IO ()
printPartOneAndTwo part1 part2 = do
  printf ("Part one " ++ show part1 ++ "\n")
  printf ("Part two " ++ show part2 ++ "\n")

readFileAsList :: FilePath -> IO [String]
readFileAsList fileName = do
  contents <- readFile fileName
  let lns = lines contents
  return lns

splitListAt :: Eq a => a -> [a] -> [[a]]
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

asInt :: String -> Int
asInt x = read x :: Int

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index item list =
  let (a, _ : b) = splitAt index list
   in a ++ item : b