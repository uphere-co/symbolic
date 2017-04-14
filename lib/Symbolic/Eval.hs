{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Eval where

import           Control.Lens               (view, _1)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.List                  (foldl')
import           Data.MemoTrie
import           Data.Vector.Storable       (Storable,(!))
import qualified Data.Vector.Storable as VS
--
import           Symbolic.Type
import           Symbolic.Util
--

mkA = Args . HM.fromList . map (\(s,v) -> (mkSym s,v))

-- mkA_ = Args . HM.fromList . map (\(s,lst) -> (mkSym s,VS.fromList lst))

evalVar :: (Num a, VS.Storable a) => Args a -> IdxPoint -> Variable -> a
evalVar args ip (V s is) = let i's = map (flip justLookupL ip . indexName) is
                               vs = justLookup s (argMap args)
                           in vs ! flatIndex is i's

evalDelta :: (Num a) => [(IndexSymbol,Int)] -> IndexSymbol -> IndexSymbol -> a
evalDelta ip i j = let i' = justLookupL i ip
                       j' = justLookupL j ip
                   in if i' == j' then 1 else 0 

evalCDelta :: (Num a) => [(IndexSymbol,Int)] -> Index -> [[Index]] -> Int -> a
evalCDelta ip i iss p =
  let i' = index0base i (justLookupL (indexName i) ip)
      js = iss !! (p-1)
      vs = map (\j -> let n = indexName j in justLookupL n ip) js
      j' = flatIndexDisjoint iss (partNth p vs) 
  in if i' == j' then 1 else 0

evalKDelta :: (Num a) => [(IndexSymbol,Int)] -> KDelta -> a
evalKDelta ip (Delta (i,_,_) (j,_,_)) = evalDelta ip i j
evalKDelta ip (CDelta i iss p)        = evalCDelta ip i iss p

eval :: ( Num a, Storable a, HasTrie a
        , ?expHash :: Exp a :->: Hash
        , ?functionMap :: FunctionMap a
        ) =>
        HashMap Hash (MExp a)
     -> (Args a,IdxPoint,Exp a)
     -> a
eval _ (_,_,Zero)         = 0
eval _ (_,_,One)          = 1
eval _ (_,_,Val n)        = n
eval _ (args,ip,Var v)    = evalVar args ip v
eval m (args,ip,Mul hs ds)   = let es = map (mexpExp . flip justLookup m) hs
                                   v1 = foldl' (*) 1 (map (\e->eval m (args,ip,e)) es)
                                   v2 = foldl' (*) 1 (map (evalKDelta ip) ds)
                               in v1*v2
eval m (args,ip,Add hs)   = let es = map (mexpExp . flip justLookup m) hs
                            in foldl' (+) 0 (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Fun s hs) = let es = map (mexpExp . flip justLookup m) hs
                                f = justLookup s ?functionMap      
                            in f (map (\e->eval m (args,ip,e)) es)
eval m (args,ip,Sum is h) = let idx1lst (idx,start,end) = [(idx,v)| v <- [start..end]]
                                sumip = traverse idx1lst is                
                                ip' = map (ip ++) sumip
                                e = justLookup h m
                            in (foldl' (+) 0 . map (\i -> eval m (args,i,mexpExp e))) ip'
eval m (args,ip,Concat idx hs) = select es di
  where es = map (flip justLookup m) hs
        i' = index0base idx (justLookupL (view _1 idx) ip)
        iss = map (HS.toList . mexpIdx) es 
        di = splitIndexDisjoint iss i'
        select (x:_)  (L i)
          = let ii = zipWith (\(k,_,_) v -> (k,v)) (HS.toList (mexpIdx x)) i
            in eval m (args,ii,mexpExp x)
        select (_:xs) (R d) = select xs d
        select [] _ = error "empty list in eval, Concat, select"


-- | simple evaluation without complex memoization
seval :: ( Storable a, HasTrie a, Num a
         , ?expHash :: Exp a :->: Hash
         , ?functionMap :: FunctionMap a
         ) =>
         Args a -> IdxPoint -> MExp a -> a
seval a ip e = eval (mexpMap e) (a,ip,mexpExp e)

