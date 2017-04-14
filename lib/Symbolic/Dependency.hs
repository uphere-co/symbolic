{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.Dependency where

import qualified Data.Array          as A
import           Data.Graph
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS          
import           Data.MemoTrie
--
import Symbolic.Type

daughters :: Exp a -> [Hash]
daughters Zero           = []
daughters One            = []
daughters (Val _)        = []
daughters (Var _)        = []
daughters (Add hs)       = hs
daughters (Mul hs _)       = hs
daughters (Fun _ hs)     = hs
daughters (Sum _ h1)     = [h1]
daughters (Concat _ hs)  = hs

mkDepEdges :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> [(Hash,Hash)]
mkDepEdges e = let e1 = mexpExp e
                   h1 = untrie ?expHash e1
                   m1 = mexpMap e
                   hs = daughters e1
                   lst = map (h1,) hs
                   lsts = map (mkDepEdges . flip justLookup m1) hs 
               in concat (lst:lsts)

mkDepEdgesNoSum :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> [(Hash,Hash)]
mkDepEdgesNoSum e =
  let e1 = mexpExp e
  in if isSumOrConcat e1
     then []
     else  
       let h1 = untrie ?expHash e1
           m1 = mexpMap e
           hs = daughters e1
           lst = map (h1,) hs
           lsts = map (mkDepEdgesNoSum . flip justLookup m1) hs 
       in concat (lst:lsts)

mkDepGraph :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> (HashMap Hash Vertex,Table Hash, Graph)
mkDepGraph e = let edgs = mkDepEdges e
                   hs = HS.toList . HS.fromList . concatMap (\(i,j) -> [i,j]) $ edgs
                   hmap = HM.fromList (zip hs [1..])
                   n = length hs
                   arr = A.listArray (1,n) hs
                   edgs' :: [(Vertex,Vertex)]
                   edgs' = flip map edgs $ \(i,j) -> let i' = justLookup i hmap; j' = justLookup j hmap; in (i',j')
               in (hmap,arr,buildG (1,n) edgs')

mkDepGraphNoSum :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a -> (HashMap Hash Vertex,Table Hash, Graph)
mkDepGraphNoSum e =
  let edgs = mkDepEdgesNoSum e
      hs = HS.toList . HS.fromList . concatMap (\(i,j) -> [i,j]) $ edgs
      hmap = HM.fromList (zip hs [1..])
      n = length hs
      arr = A.listArray (1,n) hs
      edgs' :: [(Vertex,Vertex)]
      edgs' = flip map edgs $ \(i,j) -> let i' = justLookup i hmap; j' = justLookup j hmap; in (i',j')
  in (hmap,arr,buildG (1,n) edgs')

