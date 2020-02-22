import Puzzles

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners as RS

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "length' == length"
      $ \list -> length' (list :: [Integer]) == length (list)
  , SC.testProperty "or' == or"
      $ \list -> or' (list :: [Bool]) == or (list)
  , SC.testProperty "elem' == elem"
      $ \string -> elem' 'c' (string :: String) == elem 'c' string
  , SC.testProperty "map' == map"
      $ \list -> map' (*2) (list :: [Int]) == map (*2) list
  , SC.testProperty "reverseR == reverse"
      $ \list -> reverseR (list :: [Int]) == reverse list
  , SC.testProperty "reverseL == reverse"
      $ \list -> reverseL (list :: [Int]) == reverse list
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "length' == length"
      $ \list -> length' (list :: [Integer]) == length (list)
  , QC.testProperty "or' == or"
      $ \list -> or' (list :: [Bool]) == or (list)
  , QC.testProperty "elem' == elem"
      $ \e l -> elem' e (l :: [Int]) == elem e l
  , QC.testProperty "elem' == elem"
      $ \e l -> elem' e (l :: String) == elem e l
  , QC.testProperty "map' == map"
      $ \l -> map' reverse [(l :: String)] == map reverse [l]
  , QC.testProperty "plusplus == ++"
      $ \l1 l2 -> (l1 :: [Int]) `plusplus` (l2 :: [Int]) == l1 ++ l2
  , QC.testProperty "reverseR == reverse"
      $ \l -> reverseR (l :: String) == reverse l
  , QC.testProperty "reverseL == reverse"
      $ \l -> reverseL (l :: String) == reverse l
  ]

unitTests = testGroup "Unit tests"
  [ testCase "length' []"
      $ length' [] @?= length []
  , testCase "or' []"
      $ or' [] @?= or []
  , testCase "elem' 1 []"
      $ elem' 1 [] @?= elem 1 []
  , testCase "elem' 'a' \"\""
      $ elem' 'a' "" @?= elem 'a' ""
  , testCase "elem' True [False]"
      $ elem' True [False] @?= elem True [False]
  , testCase "map' (recip . negate) [1,4,-5,0.1]"
      $ map' (recip . negate) [1,4,-5,0.1]
      @?= map (recip . negate) [1,4,-5,0.1]
  , testCase "\"Hello \" plusplus \"world\""
      $ "Hello " `plusplus` "world" @?= "Hello " ++ "world"
  , testCase "isPalindrome \"racecar\""
      $ isPalindrome "racecar" @?= True
  , testCase "isPalindrome \"Racecar\""
      $ isPalindrome "Racecar" @?= False
  , testCase "isPalindrome [1, 1, 2]"
      $ isPalindrome [1, 1, 2] @?= False
  , testCase "isPalindrome [1, 1, 1]"
      $ isPalindrome [1, 1, 1] @?= True
  , testCase "take 5 $ fibonacci"
      $ (take 5 $ fibonacci) @?= [0, 1, 1, 2, 3]
  ]
