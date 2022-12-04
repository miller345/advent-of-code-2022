import Data.List (intersect)
import Utils (printPartOneAndTwo, readFileAsList, splitListAt)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/04.txt"
  let input = map parseLine lines
  printPartOneAndTwo (part1 input) (part2 input)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line =
  let pair = splitListAt ',' line
      arr = map (\item -> map (\x -> read x :: Int) (splitListAt '-' item)) pair
      first = head arr
      sec = last arr
   in ((head first, last first), (head sec, last sec))

doesFullyContain :: ((Int, Int), (Int, Int)) -> Bool
doesFullyContain ((a, b), (c, d)) =
  -- Let left arg (x) have the greatest range
  let swap = (d - c) > (b - a) -- true if need swapping
      x0 = if swap then c else a
      x1 = if swap then d else b
      y0 = if swap then a else c
      y1 = if swap then b else d
   in (y0 >= x0) && (y1 <= x1)

doesOverlap :: ((Int, Int), (Int, Int)) -> Bool
doesOverlap ((a, b), (c, d)) = not (null (intersect [a .. b] [c .. d]))

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 input = length (filter id (map doesFullyContain input))

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 input = length (filter id (map doesOverlap input))
