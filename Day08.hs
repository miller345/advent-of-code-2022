import Data.Char (digitToInt)
import Data.List (transpose)
import Utils (asInt, getAllCoords, printPartOneAndTwo, readFileAsList)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/08.txt"
  let rows = map (map digitToInt) lines
  printPartOneAndTwo (part1 rows) (part2 rows)

type Rows = [[Int]]

type Coord = (Int, Int)

isVisible :: Rows -> Coord -> Bool
isVisible rows (x, y) =
  let (left, _ : right) = splitAt x (rows !! y)
      cols = transpose rows
      (up, _ : down) = splitAt y (cols !! x)
      val = (rows !! y) !! x
   in (x == 0)
        || (y == 0)
        || (x == length cols - 1)
        || (y == length rows - 1)
        || (val > maximum left)
        || (val > maximum right)
        || (val > maximum up)
        || (val > maximum down)

part1 :: Rows -> Int
part1 rows =
  let cols = transpose rows
      width = length $ head rows
      height = length $ head cols
      xs = [0 .. width - 1]
      ys = [0 .. height - 1]
      coords = getAllCoords xs ys
   in length $ filter id (map (isVisible rows) coords)

getScore :: Rows -> Coord -> Int
getScore rows (x, y) =
  let (left, _ : right) = splitAt x (rows !! y)
      cols = transpose rows
      (up, _ : down) = splitAt y (cols !! x)
      val = (rows !! y) !! x
      leftScore = getViewCount val (reverse left)
      rightScore = getViewCount val right
      upScore = getViewCount val (reverse up)
      downScore = getViewCount val down
   in leftScore * rightScore * upScore * downScore
  where
    getViewCount val trees = min (length (takeWhile (< val) trees) + 1) (length trees)

part2 :: Rows -> Int
part2 rows =
  let cols = transpose rows
      width = length $ head rows
      height = length $ head cols
      xs = [1 .. width - 2] -- ignore edges
      ys = [1 .. height - 2]
      coords = getAllCoords xs ys
   in maximum (map (getScore rows) coords)
