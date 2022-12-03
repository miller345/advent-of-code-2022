import Data.List (elemIndex, intersect)
import Utils (chunks, printPartOneAndTwo, readFileAsList, splitListInHalf)

main :: IO ()
main = do
  input <- readFileAsList "inputs/03.txt"
  printPartOneAndTwo (part1 input) (part2 input)

getPriority :: Char -> Int
getPriority x =
  let arr = ['a' .. 'z'] ++ ['A' .. 'Z']
      index = elemIndex x arr
   in case index of
        Just n -> n + 1
        Nothing -> error "could not get priority"

getCommonChar2 :: (String, String) -> Char
getCommonChar2 (a, b) = head (intersect a b)

getCommonChar3 :: (String, String, String) -> Char
getCommonChar3 (a, b, c) = head (intersect (intersect a b) c)

part1 :: [String] -> Int
part1 _input = let input = map splitListInHalf _input in sum (map (getPriority . getCommonChar2) input)

part2 :: [String] -> Int
part2 input =
  let groups = chunks 3 input
   in sum
        ( map
            ( \group ->
                let x = group !! 0
                    y = group !! 1
                    z = group !! 2
                 in getPriority (getCommonChar3 (x, y, z))
            )
            groups
        )
