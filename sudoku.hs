import Data.List
import Debug.Trace
import Data.Maybe
import Data.List.Split

empty :: Int
empty = 0

getRow :: [Int] -> Int -> [Int]
getRow ns i = drop (i*9) . take (i*9+9) $ ns

getCol :: [Int] -> Int -> [Int]
getCol ns i = map (\n -> ns !! n) idxes
    where idxes = map (+i) [0,9..72]

getReg :: [Int] -> Int -> [Int]
getReg ns i = map (\n -> ns !! n) surroundIdx
    where start = (i `div` 3) * 27  + (i `mod` 3) *3
          getSurround n = concat $ map (\s -> [s..s+2]) [n,n+9,n+18]
          surroundIdx = getSurround start

isSudokuPossible :: [Int] -> Bool
isSudokuPossible gm = not $ or [isPosDead pos | pos <- allZero]
  where allZero = allEmptyPos gmap
        isPosDead p = findPossibleAtPos gmap p == []
        gmap = fillOnes gm

fillOnes :: [Int] -> [Int]
fillOnes gm
  | allDecidedP == [] = gm
  | otherwise = fillOnes $ repl gm (head allDecidedP) val
  where allZero = allEmptyPos gm
        isDecided p = length(findPossibleAtPos gm p) == 1
        allDecidedP = filter isDecided allZero
        val = head $ findPossibleAtPos gm $ head allDecidedP

getMap :: String -> [Int]
getMap str = map (\s -> read s :: Int) rawChar
    where rawChar = concat $ map (splitOn ",") (words str)

repl :: [Int] -> Int -> Int -> [Int]
repl orig idx new = lft ++ [new] ++ (tail rht)
    where (lft,rht) = splitAt idx orig

allNum :: [Int]
allNum = [1..9]

allEmptyPos :: [Int] -> [Int]
allEmptyPos ns = [a | a <- [0..80] , ns !! a == 0]

getAllPossibleSubMap :: [Int] -> [[Int]]
getAllPossibleSubMap m = [s | s <- allS , isSudokuPossible s]
    where allS = [ repl m pos val | pos <- allPos, val <- (findPossibleAtPos m pos) ]
          allPos = allEmptyPos m

findPossibleAtPos :: [Int] -> Int -> [Int]
findPossibleAtPos gmap pos = ((allNum \\ rowCon) \\ colCon) \\ regCon
  where rowCon = getRow gmap $ rowIdx pos
        colCon = getCol gmap $ colIdx pos
        regCon = getReg gmap $ regIdx pos

rowIdx :: Int -> Int
rowIdx n = n `div` 9

colIdx :: Int -> Int
colIdx n = n `mod` 9

regIdx :: Int -> Int
regIdx n = 3 * (rowIdx n `div` 3) + (colIdx n) `div` 3


solveSodoku :: [Int] -> [Int]
solveSodoku map = head $ filter (\p -> length p > 0) (solveSodoku_Depth $ getAllPossibleSubMap map)

solveSodoku_Depth :: [[Int]] -> [[Int]]
solveSodoku_Depth [] = [[]]
solveSodoku_Depth [[]] = [[]]
solveSodoku_Depth (n:ns)
  | (length (filter (\p -> p==0) n )) > 0 = (solveSodoku_Depth $ getAllPossibleSubMap n) ++ rest
  | otherwise = [n]
    where rest = solveSodoku_Depth ns

examplePuzzle :: [Int]
examplePuzzle = [5, 3, 0,  0, 7, 0,  0, 0, 0,
                 6, 0, 0,  1, 9, 5,  0, 0, 0,
                 0, 9, 8,  0, 0, 0,  0, 6, 0,

                 8, 0, 0,  0, 6, 0,  0, 0, 3,
                 4, 0, 0,  8, 0, 3,  0, 0, 1,
                 7, 0, 0,  0, 2, 0,  0, 0, 6,

                 0, 6, 0,  0, 0, 0,  2, 8, 0,
                 0, 0, 0,  4, 1, 9,  0, 0, 5,
                 0, 0, 0,  0, 8, 0,  0, 7, 0]

testDepth :: [[Int]] -> Int -> Int
testDepth [] count = count
testDepth (n:ns) count
  | 0 `elem` n = testDepth (getAllPossibleSubMap n) (count +1)
  | otherwise = count

printSudoku :: [Int] -> String
printSudoku ns = "+++\n" ++ (concat $ insertEveryThird lines "---+---+---\n")
  where lines = map printOneLine nns
        nns = chunksOf 9 ns

printOneLine :: [Int] -> String
printOneLine ns = insertEveryThird strs '|' ++ ['\n']
  where strs = concat $ map show ns

insertEveryThird :: [a] -> a -> [a]
insertEveryThird (a:b:c:d:e:f:x:y:z:[]) extra = a:b:c:extra:d:e:f:extra:x:y:z:[]

main = do
    str <- getContents
    let gmap = getMap str
    putStrLn $ show $ gmap
    putStrLn $ show $ findPossibleAtPos gmap 8
    putStrLn $ show $ solveSodoku gmap

