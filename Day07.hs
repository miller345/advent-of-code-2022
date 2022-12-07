import Data.Char (isDigit)
import Data.List (intercalate, isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Utils (asInt, printPartOneAndTwo, readFileAsList, splitListAtItem)

main :: IO ()
main = do
  lines <- readFileAsList "inputs/07.txt"
  let fs = parseFileSystem lines
  printPartOneAndTwo (part1 fs) (part2 fs)

data File = File {name :: String, size :: Int} deriving (Show)

data Dir = Dir {path :: String, files :: [File], dirs :: [String]} deriving (Show)

type FileSystem = Map.Map String Dir

data LineType = CD | LS | DIR | FILE deriving (Show, Eq)

getLineType :: String -> LineType
getLineType line
  | line == "$ ls" = LS
  | "$ cd" `isInfixOf` line = CD
  | "dir " `isInfixOf` line = DIR
  | isDigit $ head line = FILE
  | otherwise = error "unknown line type"

calculateWorkingDirs :: [String] -> [String]
calculateWorkingDirs lines =
  map (\x -> "/" ++ intercalate "/" x) $
    foldl
      ( \dirs line ->
          let prevDir = last dirs
           in if getLineType line == CD
                then
                  let dirName = last (splitListAtItem ' ' line)
                   in if dirName == "/" then [[]] else if dirName == ".." then dirs ++ [init prevDir] else dirs ++ [prevDir ++ [dirName]]
                else dirs ++ [prevDir]
      )
      []
      lines

getDirNameFromLine :: String -> String
getDirNameFromLine line = last $ words line

getFileFromLine :: String -> File
getFileFromLine line =
  let size = asInt $ head $ words line
      name = last $ words line
   in File {name = name, size = size}

parseFileSystem :: [String] -> FileSystem
parseFileSystem lines =
  let workingDirs = calculateWorkingDirs lines
      zipped = zip workingDirs lines
      filtered = filter (\(_, line) -> let lineType = getLineType line in lineType == FILE || lineType == DIR) zipped
   in foldl
        ( \fs (wd, line) ->
            let path = wd
                lineType = getLineType line
                oldDir = Data.Maybe.fromMaybe Dir {path = wd, files = [], dirs = []} (Map.lookup wd fs)
                oldFiles = files oldDir
                newFiles = if lineType == FILE then getFileFromLine line : oldFiles else oldFiles
                oldDirs = dirs oldDir
                newDirs = if lineType == DIR then getDirNameFromLine line : oldDirs else oldDirs
                newDir = Dir {path = wd, files = newFiles, dirs = newDirs}
             in Map.insert wd newDir fs
        )
        Map.empty
        filtered

getTotalDirSize :: FileSystem -> String -> Int
getTotalDirSize fs dirName =
  let dir = case Map.lookup dirName fs of Just x -> x; Nothing -> error ("cannot find " ++ dirName)
      fileTotal = sum $ map size (files dir)
      dirTotal =
        sum $
          map
            (\d -> let p = (if dirName == "/" then "" else dirName) ++ "/" ++ d in getTotalDirSize fs p)
            (dirs dir)
   in fileTotal + dirTotal

part1 :: FileSystem -> Int
part1 fs =
  let totals = map (getTotalDirSize fs) (Map.keys fs)
      filtered = filter (<= 100000) totals
   in sum filtered

part2 :: FileSystem -> Int
part2 fs =
  let freeSpace = 70000000 - getTotalDirSize fs "/"
      additionalSpaceRequired = 30000000 - freeSpace
      totals = map (getTotalDirSize fs) (Map.keys fs)
      filtered = filter (>= additionalSpaceRequired) totals
   in minimum filtered
