import Data.List (intersect)
import Utils (asInt, printPartOneAndTwo, readFileAsList, splitListAtItem)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/04.txt"
  let input = map parseLine lines
  printPartOneAndTwo (part1 input) (part2 input)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine line =
  let pair = splitListAtItem ',' line
      arr = map (map asInt . splitListAtItem '-') pair
      first = head arr
      sec = last arr
   in ((head first, last first), (head sec, last sec))

doesFullyContain :: ((Int, Int), (Int, Int)) -> Bool
doesFullyContain ((a, b), (c, d)) =
  let smallest = min (length [a .. b]) (length [c .. d])
   in length ([a .. b] `intersect` [c .. d]) == smallest

doesOverlap :: ((Int, Int), (Int, Int)) -> Bool
doesOverlap ((a, b), (c, d)) = not $ null $ [a .. b] `intersect` [c .. d]

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 input = length $ filter id (map doesFullyContain input)

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 input = length $ filter id (map doesOverlap input)
