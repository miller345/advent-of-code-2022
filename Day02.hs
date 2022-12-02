import Utils (printPartOneAndTwo, readFileAsList)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/02.txt"
  let input = map words lines
  printPartOneAndTwo (part1 input) (part2 input)

convertXyz :: String -> String -- convert incorrectly assumed xyz to abc
convertXyz xyz = case xyz of "X" -> "A"; "Y" -> "B"; "Z" -> "C"; a -> a

getItemScore :: String -> Integer
getItemScore _x = let x = convertXyz _x in case x of "A" -> 1; "B" -> 2; "C" -> 3

getMyOutcomeScore :: [String] -> Integer
getMyOutcomeScore [them, _me] = let me = convertXyz _me in if me == them then 3 else (if me == getWinningMove them then 6 else 0)

getWinningMove :: String -> String
getWinningMove them = case them of "A" -> "B"; "B" -> "C"; "C" -> "A"

getLosingMove :: String -> String
getLosingMove them = case them of "A" -> "C"; "B" -> "A"; "C" -> "B"

getRoundScore :: [String] -> Integer
getRoundScore a = getItemScore (last a) + getMyOutcomeScore a

applyStrategy :: [String] -> [String] -- work out what the round will be after applying the strategy
applyStrategy [abc, xyz] = let myAbc = case xyz of "X" -> getLosingMove abc; "Y" -> abc; "Z" -> getWinningMove abc in [abc, myAbc]

part1 :: [[String]] -> Integer
part1 input = sum (map getRoundScore input)

part2 :: [[String]] -> Integer
part2 input = part1 (map applyStrategy input)
