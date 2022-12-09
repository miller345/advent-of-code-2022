import Data.List (nub)
import Utils (asInt, printPartOneAndTwo, readFileAsList)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/09.txt"
  let input = parse lines
  printPartOneAndTwo (part1 input) (part2 input)

parse :: [String] -> [Instruction]
parse = map (\line -> let [a, b] = words line in (a, asInt b))

type Instruction = (String, Int)

type Coord = (Int, Int)

data State = State {h :: Coord, t :: Coord} deriving (Show)

type State2 = (Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord, Coord)

moveHeadSingle :: String -> Coord -> Coord
moveHeadSingle dir (x, y)
  | dir == "U" = (x, y + 1)
  | dir == "D" = (x, y - 1)
  | dir == "L" = (x - 1, y)
  | dir == "R" = (x + 1, y)

moveTailSingle :: Coord -> Coord -> Coord
moveTailSingle (headX, headY) (tailX, tailY)
  -- same row
  | headY == tailY =
      let newX
            | (headX - tailX) == 2 = tailX + 1
            | (tailX - headX) == 2 = tailX - 1
            | otherwise = tailX
       in (newX, tailY)
  -- same col
  | headX == tailX =
      let newY
            | (headY - tailY) == 2 = tailY + 1
            | (tailY - headY) == 2 = tailY - 1
            | otherwise = tailY
       in (tailX, newY)
  -- touching diagonal
  | abs (tailX - headX) == 1 && abs (tailY - headY) == 1 = (tailX, tailY)
  -- gap diagonal
  | otherwise = let isAbove = headY > tailY; isRight = headX > tailX in (if isRight then tailX + 1 else tailX - 1, if isAbove then tailY + 1 else tailY - 1)

move :: String -> State -> State
move dir s =
  let newH = moveHeadSingle dir (h s)
      newT = moveTailSingle newH (t s)
   in (State {h = newH, t = newT})

expandInstructions :: [Instruction] -> [String]
expandInstructions = foldl (\acc (dir, n) -> acc ++ (take n (repeat dir))) []

part1 :: [Instruction] -> Int
part1 instrs =
  let dirs = expandInstructions instrs
      states = foldl (\states dir -> states ++ [move dir (last states)]) [(State {h = (0, 0), t = (0, 0)})] dirs
   in length $ nub $ map t states

move2 :: String -> State2 -> State2
move2 dir (a, b, c, d, e, f, g, h, i, j) =
  let newA = moveHeadSingle dir a
      newB = moveTailSingle newA b
      newC = moveTailSingle newB c
      newD = moveTailSingle newC d
      newE = moveTailSingle newD e
      newF = moveTailSingle newE f
      newG = moveTailSingle newF g
      newH = moveTailSingle newG h
      newI = moveTailSingle newH i
      newJ = moveTailSingle newI j
   in (newA, newB, newC, newD, newE, newF, newG, newH, newI, newJ)

part2 :: [Instruction] -> Int
part2 instrs =
  let dirs = expandInstructions instrs
      states = foldl (\states dir -> states ++ [move2 dir (last states)]) [((0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0))] dirs
   in length $ nub $ map (\(_, _, _, _, _, _, _, _, _, t) -> t) states