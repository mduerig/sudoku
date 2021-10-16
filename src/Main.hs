module Main where

s1 :: [Int]
s1 = [
    4,6,5, 3,0,2, 9,0,0,
    0,1,0, 0,4,0, 0,0,3,
    9,0,0, 0,0,0, 0,4,2,

    7,8,0, 6,0,0, 0,0,0,
    0,0,0, 0,5,0, 0,0,0,
    0,0,0, 0,0,7, 0,3,6,

    1,9,0, 0,0,0, 0,0,5,
    2,0,0, 0,6,0, 0,1,0,
    0,0,4, 1,0,3, 8,2,9
    ]

s2 :: [Int]
s2 = [
    0,0,6, 9,0,0, 8,0,0,
    2,0,0, 0,0,0, 1,0,0,
    0,0,0, 0,0,0, 0,0,0,

    0,5,0, 0,3,7, 0,0,0,
    4,0,0, 0,0,0, 6,0,0,
    0,0,0, 0,5,0, 0,0,0,

    9,3,0, 0,0,0, 0,0,5,
    0,0,0, 6,0,0, 0,2,0,
    0,0,0, 1,0,0, 0,0,0
    ]

main :: IO ()
main =
  let
    solution = solve s1
  in do
    print solution
    print (solution == [[4,6,5,3,1,2,9,8,7,8,1,2,7,4,9,5,6,3,9,3,7,5,8,6,1,4,2,7,8,9,6,3,4,2,5,1,3,4,6,2,5,1,7,9,8,5,2,1,8,9,7,4,3,6,1,9,3,4,2,8,6,7,5,2,7,8,9,6,5,3,1,4,6,5,4,1,7,3,8,2,9]])


solve :: [Int] -> [[Int]]
solve sudoku = fst $ iterate nextField ([[]], sudoku) !! 81

nextField :: ([[Int]], [Int]) -> ([[Int]], [Int])
nextField (solved, unsolved) =
  (do
    s <- solved
    n <- filter (permissible s unsolved) [1..9]
    return (s ++ [n]), tail unsolved)

permissible :: [Int] -> [Int] -> Int -> Bool
permissible solved unsolved n =
  let
    pos = length solved
    board = (solved ++ unsolved) `zip` [0..]
  in
    head unsolved == n ||
    head unsolved == 0 &&
    not (
      inSameRow pos n board ||
      inSameCol pos n board ||
      inSameBox pos n board)

inSameRow :: Int -> Int -> [(Int, Int)] -> Bool
inSameRow pos n board =
  let
    row n = n `div` 9
    rowN = [m | (m, i) <- board, row i == row pos]
  in
    n `elem` rowN

inSameCol :: Int -> Int -> [(Int, Int)] -> Bool
inSameCol pos n board =
  let
    col n = n `mod` 9
    colN = [m | (m, i) <- board, col i == col pos]
  in
    n `elem` colN

inSameBox :: Int -> Int -> [(Int, Int)] -> Bool
inSameBox pos n board =
  let
    boxRow n = n `div` 27
    boxCol n = (n `div` 3) `mod` 3
    box = [m | (m, i) <- board, boxRow i == boxRow pos && boxCol i == boxCol pos]
  in
    n `elem` box
