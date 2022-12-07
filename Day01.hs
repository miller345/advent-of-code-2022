import Data.List (sort)
import Utils (asInt, printPartOneAndTwo, readFileAsList, splitListAtItem)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/01.txt"
  let input = parse lines
  printPartOneAndTwo (part1 input) (part2 input)

parse :: [String] -> [[Int]]
parse lines = let grouped = splitListAtItem "" lines in map (map asInt) grouped

part1 :: [[Int]] -> Int
part1 input = maximum (map sum input)

part2 :: [[Int]] -> Int
part2 input = sum (drop (length input - 3) (sort (map sum input)))