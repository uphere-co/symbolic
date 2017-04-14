{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Simple where

import           Data.Foldable             (forM_)
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable as VS
import           Text.Printf
--
-- import           Symbolic.CodeGen.C
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Simplify
import           Symbolic.Type
import           Symbolic.Util
--

idxi, idxj, idxI, idxk, idxm, idxn :: Index
idxi = ("i",1,2)
idxj = ("j",1,2)

idxI = ("I",1,4)

idxk = ("k",1,2)
idxm = ("m",1,2)
idxn = ("n",1,2)


test2 :: IO ()
test2 = do
    let ?expHash = trie hash
    let exp2 :: MExp Int
        exp2 = power 3 varx -- power 10 (x `add'` y)
    
    digraph (exp2 :: MExp Int)

test8 :: IO ()
test8 = do
  let ?expHash = trie hash
  let e1 = add' [x_ [idxi], zero,  y_ [idxi], x_ [idxj], zero] 
  printf "e1 = %s\n" ((prettyPrint . exp2RExp) (e1 ::  MExp Int) :: String)

  let e2 = mul' [x_ [idxi], one,  y_ [idxj], x_ [idxi], one] 
  printf "e2 = %s\n" ((prettyPrint . exp2RExp) (e2 ::  MExp Int) :: String)
  -- digraph e2

  let e3 = mul [varx, varx, mul [varx, varx , varx] , varx]
      de3 = (sdiff HM.empty (V (mkSym "x") []) e3 ::  MExp Int)
  printf "e3 = %s\n" ((prettyPrint . exp2RExp) (e3 ::  MExp Int) :: String)
  printf "d(e3)/dx = %s\n" ((prettyPrint . exp2RExp) de3  :: String)
  digraph de3


test10 :: IO ()
test10 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let e = mul [varx, vary] :: MExp Int
      args = Args (HM.fromList [(mkSym "x",VS.fromList [2])
                               ,(mkSym "y",VS.fromList [3])])
  printf "e = %s\n"  ((prettyPrint . exp2RExp) e :: String)
  
  printf "val(e) = %d\n" (eval (mexpMap e) (args,[],mexpExp e))


test12 :: IO ()
test12 = do
  let ?expHash = trie hash
      ?functionMap = HM.fromList [ ("f", \[x,y] -> x*x + y*y)
                                 , ("f_1", \[x,_y] -> 2*x)
                                 , ("f_2", \[_x,y] -> 2*y) ]
  let e1 :: MExp Int
      e1 = add' [mul' [val 2,varx],vary]
      fe1 = fun "f" [e1,varx]
      dm = HM.fromList [ ("y",["x"]) ]
      dfe1 = sdiff dm (V (mkSym "x") []) fe1
  printf "fe1 = %s\n"  ((prettyPrint . exp2RExp) fe1 :: String)
  printf "d(fe1)/dx = %s\n" ((prettyPrint . exp2RExp) dfe1 :: String)
  --  digraph dfe1
  let args = Args (HM.fromList [(mkSym "x",VS.fromList [2])
                               ,(mkSym "y",VS.fromList [3])
                               ,(Deriv "y" "x", VS.fromList [4])
                               ])
  
  printf "dfe1/dx(2,3)) = %d\n" (seval args [] dfe1)



    
test18 :: IO ()
test18 = do
  let ?expHash = trie hash
      ?functionMap = HM.fromList [("temp", (/100.0) . head)]
  let exp1 :: MExp Float
      exp1 = fun "temp" [concat_ idxI [ mul [ x_ [idxi], x_ [idxi] ]  , mul [ y_ [idxj], x_ [idxj] ] ]]
  putStr "f = "
  prettyPrintR exp1

  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args (HM.fromList [(mkSym "x",xvals)
                               ,(mkSym "y",yvals)
                               ])
  
  forM_ [ iI | iI <- [1,2,3,4] ] $ \iI -> do
    let iptI = [("I",iI)]
    printf "val(I=%d) = %f \n" iI (seval args iptI exp1)
  
    
