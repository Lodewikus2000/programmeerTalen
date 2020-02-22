import Sudoku
import SudokuSolver as Solver

import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe

testGridStageOne :: Grid
testGridStageOne =
  [ [1,2,3,4,5,6,7,0,9]
  , [4,5,6,7,8,9,1,2,3]
  , [7,8,9,1,2,3,4,5,6]
  , [2,3,1,6,7,4,8,9,5]
  , [8,7,5,9,1,2,0,6,4]
  , [6,9,4,5,3,8,2,1,7]
  , [3,1,7,2,6,5,9,4,8]
  , [5,4,2,8,9,7,6,3,1]
  , [9,6,8,3,4,1,5,7,2]
  ]

testGridStageTwo :: Grid
testGridStageTwo =
  [ [1,1,1,0,0,0,7,0,9]
  , [4,5,6,7,8,9,1,2,3]
  , [7,8,9,1,2,3,4,5,6]
  , [2,3,1,6,7,4,8,9,5]
  , [8,7,5,9,1,2,0,6,4]
  , [6,9,4,5,3,8,2,1,7]
  , [3,1,7,2,6,5,9,4,8]
  , [5,4,2,8,9,7,6,3,1]
  , [9,6,8,3,4,1,5,7,2]
  ]

testGridSolveSimple :: Grid
testGridSolveSimple =
  [ [1,0,3,4,5,6,7,0,9]
  , [4,5,6,7,8,9,1,2,3]
  , [7,8,9,1,2,3,0,5,6]
  , [2,3,1,6,7,4,8,9,5]
  , [8,7,5,9,0,2,3,6,4]
  , [6,9,4,5,3,8,2,0,7]
  , [3,1,7,2,6,5,9,4,8]
  , [5,0,2,8,0,7,6,0,1]
  , [9,6,8,3,4,1,0,7,2]
  ]

testGridSolveMedium :: Grid
testGridSolveMedium =
  [ [1,0,3,0,0,0,0,0,9]
  , [4,5,6,0,8,9,1,2,3]
  , [7,8,9,1,2,3,0,5,6]
  , [2,0,1,6,7,4,8,9,5]
  , [8,0,5,9,0,2,3,6,4]
  , [6,0,0,5,3,8,2,0,7]
  , [3,1,7,0,6,5,9,4,8]
  , [5,0,2,0,0,7,6,0,1]
  , [9,0,8,3,4,1,0,7,2]
  ]

testGridSolveHard :: Grid
testGridSolveHard =
  [ [0,0,3,0,0,0,0,0,9]
  , [0,5,0,0,8,0,1,0,3]
  , [0,0,9,0,0,3,0,5,0]
  , [0,0,1,6,0,4,0,9,0]
  , [8,0,5,9,0,2,3,6,4]
  , [0,0,0,5,0,8,2,0,7]
  , [3,0,7,0,6,5,9,4,8]
  , [0,0,2,0,0,7,6,0,1]
  , [0,0,8,0,0,0,0,0,0]
  ]

testGridEmpty :: Grid
testGridEmpty =
  [ [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  , [0,0,0,0,0,0,0,0,0]
  ]



createStageOneTests :: Sudoku -> TestTree
createStageOneTests sud = testGroup "Stage one"
  [ testCase "freeInRow" $ Solver.freeInRow sud 1 @?= [8]
  , testCase "freeInRow with none free" $ Solver.freeInRow sud 2 @?= []
  , testCase "freeInCol" $ Solver.freeInColumn sud 7 @?= [3]
  , testCase "freeInCol with none free" $ Solver.freeInColumn sud 1 @?= []
  , testCase "freeInSubgrid" $ Solver.freeInSubgrid sud (1, 8) @?= [8]
  , testCase "freeInSubgrid with none free" $ Solver.freeInSubgrid sud (9, 9)
      @?= []
  , testCase "freeAtPos" $ Solver.freeAtPos sud (1, 8) @?= [8]
  , testCase "freeAtPos" $ Solver.freeAtPos sud (1, 1) @?= []
  , testCase "openPositions" $ Solver.openPositions sud @?= [(1, 8), (5, 7)]
  , testCase "rowValid" $ Solver.rowValid sud 1 @?= True
  , testCase "colValid" $ Solver.colValid sud 7 @?= True
  , testCase "subgridValid" $ Solver.subgridValid sud (5, 5) @?= True
  , testCase "consistent" $ Solver.consistent sud @?= True
  , testCase "constraints" $ Solver.constraints sud
      @?= [(1, 8, [8]),(5, 7, [3])]
  ]

createStageTwoTests :: Sudoku -> TestTree
createStageTwoTests sud = testGroup "Stage two"
  [ testCase "freeInRow" $ Solver.freeInRow sud 1 @?= [2, 3, 4, 5, 6, 8]
  , testCase "freeInColumn" $ Solver.freeInColumn sud 3 @?= [3]
  , testCase "freeInSubgrid" $ Solver.freeInSubgrid sud (1, 1) @?= [2, 3]
  , testCase "freeAtPos" $ Solver.freeAtPos sud (1, 2) @?= [2]
  , testCase "openPositions" $ Solver.openPositions sud
      @?= [(1, 4), (1, 5), (1, 6), (1, 8), (5, 7)]
  , testCase "rowValid" $ Solver.rowValid sud 1 @?= False
  , testCase "colValid" $ Solver.colValid sud 2 @?= False
  , testCase "subgridValid" $ Solver.subgridValid sud (1, 1) @?= False
  , testCase "consistent" $ Solver.consistent sud @?= False
  , testCase "constraints" $ Solver.constraints sud
      @?= [(1, 4, [4]),(1, 5, [5]),(1, 6, [6]),(1, 8, [8]),(5, 7, [3])]
  ]

{- IMPORTANT! This only tests if you return the right part of maybe,
   not if its an actually solved sudoku. The solving part can be checked by
   yourself with ghci or with the tests on codegrade.
-}
createStageSolve :: TestTree
createStageSolve = testGroup "Solving"
  [ testCase "solve ease sudoku"
      $ isJust (Solver.solveSudoku $ Solver.grid2sud testGridSolveSimple)
      @?= True
  , testCase "solve medium sudoku"
      $ isJust (Solver.solveSudoku $ Solver.grid2sud testGridSolveMedium)
      @?= True
  , testCase "solve hard sudoku"
      $ isJust (Solver.solveSudoku $ Solver.grid2sud testGridSolveHard)
      @?= True
  , testCase "solve invalid sudoku"
      $ isNothing (Solver.solveSudoku $ Solver.grid2sud testGridStageTwo)
      @?= True
  , testCase "solve empty sudoku"
      $ isJust (Solver.solveSudoku $ Solver.grid2sud testGridEmpty)
      @?= True
  ]

main :: IO ()
main = do
  let sudStageOne = Solver.grid2sud testGridStageOne
  let sudStageTwo = Solver.grid2sud testGridStageTwo
  let sudStageSimple = Solver.grid2sud testGridSolveSimple
  let tests = testGroup
        "Tests"
        [ createStageOneTests sudStageOne
        , createStageTwoTests sudStageTwo
        , createStageSolve
        ]
  defaultMain tests
