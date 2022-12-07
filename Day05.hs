import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf)
import Utils (asInt, printPartOneAndTwo, readFileAsList, replaceAt, splitListAtItem)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/05.txt"
  let stacks = parseStacks lines
  let instructions = parseInstructions lines
  printPartOneAndTwo (part1 instructions stacks) (part2 instructions stacks)

getStack :: [String] -> Int -> String
getStack lines n = map (!! ((4 * n) - 3)) lines

parseStacks :: [String] -> [String]
parseStacks lines =
  let filtered = filter (isInfixOf "[") lines
      stackCount = (length (head filtered) + 1) `div` 4
      extracted = map (getStack filtered) [1 .. stackCount]
   in map (filter (not . isSpace)) extracted

parseInstructions :: [String] -> [(Int, Int, Int)]
parseInstructions lines =
  let filtered = filter (isInfixOf "move") lines
   in map parseInstruction filtered

parseInstruction :: String -> (Int, Int, Int)
parseInstruction line =
  let extractNum str = asInt (filter isDigit str)
      count = head (splitListAtItem 'f' line)
      fromAndTo = last (splitListAtItem 'f' line)
      from = head (splitListAtItem 't' fromAndTo)
      to = last (splitListAtItem 't' fromAndTo)
   in (extractNum count, extractNum from, extractNum to)

applyInstruction :: Bool -> (Int, Int, Int) -> [String] -> [String]
applyInstruction rev (count, from, to) stacks =
  let fromStack = stacks !! (from - 1)
      toStack = stacks !! (to - 1)
      moving = if rev then reverse (take count fromStack) else take count fromStack
      newFromStack = drop count fromStack
      newToStack = moving ++ toStack
   in replaceAt (to - 1) newToStack (replaceAt (from - 1) newFromStack stacks)

applyInstructions :: Bool -> [(Int, Int, Int)] -> [String] -> [String]
applyInstructions rev instructions stacks =
  foldl (\acc instruction -> applyInstruction rev instruction acc) stacks instructions

part1 :: [(Int, Int, Int)] -> [String] -> [Char]
part1 instructions stacks =
  let result = applyInstructions True instructions stacks
   in map head result

part2 :: [(Int, Int, Int)] -> [String] -> [Char]
part2 instructions stacks =
  let result = applyInstructions False instructions stacks
   in map head result