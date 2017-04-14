{-# LANGUAGE ImplicitParams #-}

import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.MemoTrie
import           Data.List
import           Data.Ord
--
import           Test.Tasty
import           Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit
--
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Simplify
import           Symbolic.Type


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ properties, unitTests]


properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]

idxi = ("i",1,2)
idxj = ("j",1,2)
idxk = ("k",1,2)
idxm = ("m",1,2)
idxn = ("n",1,2)

test_1 = 
  let e = mul [x, y] :: MExp Int
      args = Args (HM.fromList [("x",2),("y",3)]) (HM.empty)
  in eval (mexpMap e) (args,[],mexpExp e)
 where 
  ?expHash = trie hash
  ?functionMap = HM.empty

test_2 = 
  let e1 :: MExp Int
      e1 = add' [mul' [val 2,x],y]
      fe1 = fun "f" [e1,x]
      dfe1 = sdiff (Simple "x") fe1
      args = Args (HM.fromList [("x",2),("y",3)]) (HM.empty)
  in seval args [] dfe1
 where
  ?expHash = trie hash
  ?functionMap = HM.fromList [ ("f", \[x,y] -> x*x + y*y)
                             , ("f_1", \[x,y] -> 2*x)
                             , ("f_2", \[x,y] -> 2*y) ]

  
unitTests = testGroup "Unit tests"
  [ testCase "f(x,y) = x * y, f (2,3) ?= 6" (test_1 @?= 6)
  , testCase "f(x,y) = x^2+y^2, fe1 = f(2*x + y, x), dfe1/dx (x=2,y=3) ?= 32" (test_2 @?= 32)
  ]
