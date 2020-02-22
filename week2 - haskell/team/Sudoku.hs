module Sudoku

where

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]]
type Sudoku = (Row, Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])
