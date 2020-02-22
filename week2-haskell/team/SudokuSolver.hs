module SudokuSolver

where

import Sudoku
import Data.List
import Data.Maybe

positions, values :: [Int]
positions = [1..9]
values    = [1..9]

blocks :: [[Int]]
blocks = [[1..3], [4..6], [7..9]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showSubgridRow :: [Value] -> String
showSubgridRow = unwords . map showDgt

{-- The chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)
property holds.
--}
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = [] -- Models the Data.List.Split implementation
chunksOf n ls = fst splitted : (chunksOf n . snd) splitted
  where splitted = splitAt n ls

showRow :: [Value] -> String
showRow sr =
  "| "
  ++
  intercalate " | " (map showSubgridRow $ chunksOf 3 sr)
  ++
  " |"

showGrid :: Grid -> String
showGrid grid =
  "+-------+-------+-------+\n"
  ++
  intercalate "\n+-------+-------+-------+\n" rows
  ++
  "\n+-------+-------+-------+"
  where rows = map (intercalate "\n") $ chunksOf 3 $ map showRow grid

sud2grid :: Sudoku -> Grid
sud2grid s =
  [ [ s (r, c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
    where pos :: [[a]] -> (Row, Column) -> a
          pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)


printSudoku :: Sudoku -> IO()
printSudoku = putStrLn . showGrid . sud2grid

-- fill in a value somewhere in the sudoku
extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) (x,y) | (r,c) == (x,y) = v
                       | otherwise      = s (x,y)

-- get all values in a row, and filter the 0 from it
-- based on the value list
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =  filter (`notElem` ingevuld) values
                 where ingevuld = [s (r, i) | i <- positions]

-- get all values in a col, and filter the 0's from it
-- based on the value list
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c =  filter (`notElem` ingevuld) values
                    where ingevuld = [s (i, c) | i <- positions]

-- get all values in a subgrid, and filter the 0's from it
-- based on the value list
freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = filter (`notElem` ingevuld) values
                        where
                            ingevuld = [s (i,j) | i <- blocks !! x,
                                                  j <- blocks !! y]
                            x = (r-1) `div` 3
                            y = (c-1) `div` 3

-- returns the values that are free at a given position
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = (freeInColumn s c `intersect` freeInRow s r)
                    `intersect` freeInSubgrid s (r,c)

-- returns the open positions within the sudoku
--  by filtering the 0's from the positions.
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = sort (filter (\(r,c) -> s (r,c) == 0)
                        [ (r,c) | c <- positions , r <- positions ])

-- checks whether there are no duplicate values in a row.
rowValid :: Sudoku -> Row -> Bool
rowValid s r = nub ingevuld == ingevuld
                where
                  ingevuld = filter (/= 0) [s (r,c) | c <- positions]

-- checks whether there are no duplicate values in a col.
colValid :: Sudoku -> Column -> Bool
colValid s c  =  nub ingevuld == ingevuld
                  where
                    ingevuld = filter (/= 0) [s (r,c) | r <- positions]

-- checks whether there are no duplicate values in a subgrid.
subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid s (r,c) = nub ingevuld == ingevuld
                        where
                          x = (r-1) `div` 3
                          y = (c-1) `div` 3
                          ingevuld = filter (/= 0)
                                     [s (i,j) | i <- blocks !! x,
                                                j <- blocks !! y]

-- checks whether the sudoku is filled in in a consistent way.
consistent :: Sudoku -> Bool
consistent s = False `notElem` ([rowValid s r | r <- positions] ++
                                [colValid s c | c <- positions] ++
                                [subgridValid s (r,c) |
                                 r <- [1,4,7], c <- [1,4,7]])

-- returns all constrains on all open spots in a given sudoku
constraints :: Sudoku -> [Constraint]
constraints s = sortBy compareConstraints constrain_list
                where constrain_list = [(i,j, freeAtPos s(i,j))
                                        | i <- positions,
                                          j <- positions, s(i,j) == 0]

-- function to compare constrains with eachother.
-- we only care about the length of the possible options in a field.
compareConstraints :: (Row, Column, [Value]) -> (Row, Column, [Value]) -> Ordering
compareConstraints (_, _, a) (_, _, b)
  | length a < length b = LT
  | length a > length b = GT
  | otherwise           = EQ

-- print a given node in the search tree
printNode :: Node -> IO()
printNode = printSudoku . fst

-- solve the suduko
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku sud | openPositions sud == [] && not (isSolution sud) = Nothing
                | otherwise                              = solve' sud cnds
                  where
                    pos       = head $ openPositions sud
                    cnds      = freeAtPos sud pos

solve' :: Sudoku -> [Int] -> Maybe Sudoku
solve' sud cnds | null cnds = Nothing
                | null $ openPositions new_sud = Just new_sud
                | isNothing solution = solve' sud $ tail cnds
                | otherwise = solution
  where
    new_sud = extend sud (x,y, cnd)
    (x,y) = head $ openPositions sud
    cnd = head cnds
    new_pos = head $ openPositions new_sud
    new_cnds = freeAtPos new_sud new_pos
    solution = solve' new_sud new_cnds


isSolution :: Sudoku -> Bool
isSolution s = consistent s && openPositions s == []

-- show the solved grid
solveAndShow :: Grid -> IO()
solveAndShow g = putStrLn "asdf"
