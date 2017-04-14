{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Rule where

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

showExp s e = printf "%s = %s\n" s ((prettyPrint . exp2RExp) e :: String)

rule1 :: IO ()
rule1 = do
  let ?expHash = trie hash
  let idxj, idxn :: Index
      idxj = ("j",1,2)
      idxn = ("n",1,2)
      original, changed :: MExp Int
      original = sum_ [idxn] (mul [one,delta idxn idxj])
      changed = sum'_ [idxn] (mul' [one,delta idxn idxj])
  showExp "original" original
  showExp "changed" changed

rule2 :: IO ()
rule2 = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let original, changed :: MExp Int
      ne1 = sum_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      ne2 = sum_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      original = add [ne1,ne2]  
      e1 = sum'_ [idxn,idxm] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn])
      e2 = sum'_ [idxm,idxn] (mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm])
      changed = add' [e1,e2]
  showExp "original" original
  showExp "changed" changed

rule3 :: IO ()
rule3 = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let original, changed :: MExp Int
      original = sum_ [idxn,idxm] (mul' [add' [x_ [idxm,idxn], y_ [idxi ,idxn]], delta idxj idxn, delta idxm idxk])
      changed = sum'_ [idxn,idxm] (mul' [add' [x_ [idxm,idxn], y_ [idxi ,idxn]], delta idxj idxn, delta idxm idxk])
  showExp "original" original
  showExp "changed" changed


rule4 :: IO ()
rule4 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)

      idxI = ("I",1,4)
      idxJ = ("J",1,4)

      original1, changed1, original2, changed2, original3, changed3 :: MExp Int
      e1 = concat_ idxI [ mul' [ x_ [idxi], x_ [idxi] ]  , mul' [ y_ [idxj], x_ [idxj] ] ]
      original1 = sum_ [idxI] (mul' [e1, delta idxI idxJ ] )
      changed1  = sum'_ [idxI] (mul' [e1, delta idxI idxJ ] )
      original2 = sum_ [idxI] (mul' [e1, delta idxi idxk ] )
      changed2  = sum'_ [idxI] (mul' [e1, delta idxi idxk ] )
      original3 = sum_ [idxI] (mul' [e1, cdelta idxI [[idxi],[idxj]] 1 ] )
      changed3  = sum'_ [idxI] (mul' [e1, cdelta idxI [[idxi],[idxj]] 1 ] )

  showExp "original1" original1
  showExp "changed1" changed1
  putStrLn "--"
  showExp "original2" original2
  showExp "changed2" changed2
  putStrLn "--"
  showExp "original3" original3
  showExp "changed3" changed3
  putStrLn "--"


norule :: IO ()
norule = do
  let idxi, idxj, idxm, idxn, idxk :: Index
      idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxk = ("k",1,2)
      idxm = ("m",1,2)
      idxn = ("n",1,2)
  let ?expHash = trie hash
  let test1,test2 :: MExp Int
      e1 = mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi ,idxk], delta idxi idxm, delta idxj idxn]
      e2 = mul' [delta idxm idxk, x_ [idxm,idxn], y_ [idxi, idxk], delta idxi idxn, delta idxj idxm]
      test1 = sum_ [idxm,idxn] (add' [e1,e2])      
      test2 = sum'_ [idxm,idxn] (add' [e1,e2])
  showExp "test1" test1
  showExp "test2" test2



