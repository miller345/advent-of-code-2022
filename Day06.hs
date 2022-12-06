import Data.List (nub)
import Utils (printPartOneAndTwo)

main :: IO ()
main = do
  line <- readFile "inputs/06.txt"
  printPartOneAndTwo (part1 line) (part2 line)

data Acc = Acc {s :: String, count :: Int, found :: Bool}

firstSetOfNDistinctCharsIsAt :: Int -> String -> Int
firstSetOfNDistinctCharsIsAt n str =
  count
    ( foldl
        ( \(Acc {s = s, count = i, found = f}) _ ->
            if f
              then (Acc {s = s, count = i, found = f})
              else
                let firstN = take n s
                    isUnique = length (nub firstN) == n
                 in (Acc {s = drop 1 s, count = i + 1, found = isUnique})
        )
        (Acc {s = str, count = 0, found = False})
        str
    )
    + n
    - 1

part1 :: String -> Int
part1 = firstSetOfNDistinctCharsIsAt 4

part2 :: String -> Int
part2 = firstSetOfNDistinctCharsIsAt 14