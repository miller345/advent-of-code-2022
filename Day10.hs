import Data.List (nub)
import Utils (asInt, printPartOneAndTwo, readFileAsList, chunks)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/10.txt"
  let state = run lines
  print (part1 state)
  let p2 = (part2 state)
  print $ p2 !! 0
  print $ p2 !! 1
  print $ p2 !! 2
  print $ p2 !! 3
  print $ p2 !! 4
  print $ p2 !! 5

type State = [Int]

noop :: State->State
noop s = let prev = last s in s ++ [prev]  

addx :: Int->State->State
addx x s = let prev = last s in s ++ [prev,prev+x]

run :: [String]->State
run = foldl (\s line->if line == "noop" then noop s else let x = asInt $ last $ words line in addx x s) [1]

part1 s = sum $ map (\n -> n*(s !! (n-1))) [20,60..220]

part2 s = chunks 40 $ map (\_n->let v=s!!(_n-1); n=_n `mod` 40 in if (v-1==n-1) || (v==n-1) || (v+1==n-1) then '#' else '.') [1..length s]