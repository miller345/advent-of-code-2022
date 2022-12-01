import Data.List (sort)
import Utils (printPartOneAndTwo, readFileAsList, splitListAt)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/01.txt"
  let input = parse lines
  printPartOneAndTwo (part1 input) (part2 input)

parse :: [String] -> [[Integer]]
parse lines = let grouped = splitListAt "" lines in map (map (\str -> read str :: Integer)) grouped

part1 :: [[Integer]] -> Integer
part1 input = maximum (map sum input)

part2 :: [[Integer]] -> Integer
part2 input = sum (drop (length input - 3) (sort (map sum input)))