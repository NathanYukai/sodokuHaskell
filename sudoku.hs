import Data.List
import Debug.Trace
import Data.Maybe
import Data.List.Split


shift :: [a] -> Int -> [a]
shift l n = drop n l ++ take n l

allRotations :: [a] -> [[a]]
allRotations l = [shift l i | i <- [0..(length l)-1]]

linearSearch :: String -> [String] -> Int -> Int
linearSearch str [] idx = -1
linearSearch str (s:source) idx
  | str == s = idx
  | otherwise = linearSearch str source (idx+1)

binarySearch :: String -> [String] -> Int -> Int
binarySearch _ [] _ = -1
binarySearch str source ptr
  | middleStr == str = middleIdx + ptr
  | middleStr > str = binarySearch str leftH ptr
  | otherwise = binarySearch str rightH (ptr+middleIdx)
    where middleIdx = length source `div` 2
          middleStr = source !! middleIdx
          (leftH,rightH) = splitAt middleIdx source


empty :: Int
empty = 0 

isSubRegionValid :: [Int] -> Bool
isSubRegionValid ns = length ns == 9 && and [b `elem` ns | b <-[1..9]]

-- just check if there are duplicated
isSubRegionPossible :: [Int] -> Bool
isSubRegionPossible ns = length (nub filled) == (length filled)
    where filled = filter (\a -> a `elem` [1..9]) ns

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

type Valid = (Bool,Bool,Bool)

isSudokuValid :: [Int] -> Valid
isSudokuValid ns 
  | (0 `elem` ns) = (False, False,False)
  | otherwise = isSudoku_ ns isSubRegionValid

isSudokuPossible :: [Int] -> Valid
isSudokuPossible ns = isSudoku_ ns isSubRegionPossible

isSudoku_ :: [Int] -> ([Int] -> Bool) -> Valid 
isSudoku_ ns fn = (allRowVal , allColVal , allRegVal)
    where allRowVal = and [fn (getRow ns i) | i <- [0..8] ]
          allColVal = and [fn (getCol ns i) | i <- [0..8] ]
          allRegVal = and [fn (getReg ns i) | i <- [0..8] ]

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

allValid :: Valid -> Bool
allValid (True,True,True) = True
allValid (_,_,_) = False

getAllPossibleSubMap :: [Int] -> [[Int]]
getAllPossibleSubMap m = [s | s <- allS m , allValid $ isSudokuPossible s]
    where allS sm = [ repl sm pos val | pos <- allPos, val <- allNum]
          allPos = allEmptyPos m

haveSolution :: [[Int]] -> Int
haveSolution ns = 
    case idx of
      (Nothing) -> -1
      (Just i) -> i
    where idx = elemIndex True valid
          valid = map (allValid . isSudokuValid) ns 

solveSodoku :: [Int] -> [Int]
solveSodoku map = head $ solveSodoku_Depth $ getAllPossibleSubMap map

solveSodoku_Depth :: [[Int]] -> [[Int]]
solveSodoku_Depth [] = []
solveSodoku_Depth (n:ns)
  | (length . filter (\p -> p==0)) n > 10 = (solveSodoku_Depth $ getAllPossibleSubMap n) ++ rest
--  | allValid (isSudokuValid n) = [n] -- ++ solveSodoku_Depth ns
  | otherwise = [n]
    where rest = solveSodoku_Depth ns

testDepth :: [[Int]] -> Int -> Int
testDepth [] count = count 
testDepth (n:ns) count 
  | 0 `elem` n = testDepth (getAllPossibleSubMap n) (count +1) 
  | otherwise = count

solveSodoku_Rec :: [[Int]] -> [Int]
solveSodoku_Rec subMaps 
  | solutionIdx == -1 = solveSodoku_Rec $ concat $ map getAllPossibleSubMap subMaps
  | otherwise = subMaps !! solutionIdx
    where solutionIdx = haveSolution subMaps

main = do
    str <- getContents
    let gmap = getMap str
    putStrLn $ show $ solveSodoku gmap

