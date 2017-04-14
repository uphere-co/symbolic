{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Fib where

import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.MemoTrie
--
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--

expfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Int :->: MExp a) -> Int -> MExp a
expfib' _ 0 = varx -- x_ ["i"]
expfib' _ 1 = vary -- y_ ["i"]
expfib' t n = let e1 = untrie t (n-1)
                  e2 = untrie t (n-2)
              in add [e1, e2]

expfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => Int -> MExp a
expfib = 
    let t = trie expfib
        extfib = expfib' t
    in extfib

dexpfib' :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => 
            (Int :->: MExp a, (Variable,Exp a) :->: MExp a)
         -> (Variable,Int) -> MExp a
dexpfib' (tfib,tdiff) (s,n) = let MExp e m _ = untrie tfib n in diff' HM.empty m tdiff (s,e)

dexpfib :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => (Variable,Int) -> MExp a
dexpfib (s,n) = let tfib = trie ffib
                    ffib = expfib' tfib
                    MExp _ m _ = untrie tfib n
                    tdiff = trie (diff' HM.empty m tdiff)
                    f = dexpfib' (tfib,tdiff) 
                in f (s,n)

{- 
eval_fib :: (HasTrie a, Num a, Floating a, ?expHash :: Exp a :->: Hash) => Args a -> IdxPoint -> Int -> EExp a
eval_fib a ip n = let tfib = trie ffib
                      ffib = expfib' tfib
                      e = mexpExp (ffib n)
                      m = mexpMap (ffib n)
                      feval = eval m teval
                      teval = trie feval 
                  in untrie teval (a,ip,e)
-}

testfib :: IO ()
testfib = do
  let ?expHash = trie hash    
  -- let lexp1 = expfib 10
  let n = 5
      lexp1 = expfib n :: MExp Int
      lexp2 = dexpfib (V (mkSym "x") [],n)
  prettyPrintR $ lexp1
  prettyPrintR $ lexp2    
  -- (printf "lexp2: %x\n" . untrie ?expHash . mexpExp) lexp2



