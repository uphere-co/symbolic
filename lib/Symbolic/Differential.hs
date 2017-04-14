{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Differential where

import           Data.Function             (fix)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import           Data.MemoTrie
--
import           Symbolic.Predefined
import           Symbolic.Simplify
import           Symbolic.Type
--

diff'
  :: forall a. (HasTrie a, Num a, ?expHash :: Exp a :->: Hash)
  => DependencyMap
  -> HashMap Hash (MExp a)
  -> ((Variable,Exp a) :->: MExp a)
  -> (Variable,Exp a) -> MExp a
diff' dm m t (s,e) =
  case e of
    Zero         -> zero 
    One          -> zero
    Val _        -> zero
    Var s'       -> dvar dm s s'
    Add hs       -> let es = map (flip justLookup m) hs
                    in add' (map (\e' -> untrie t (s,mexpExp e')) es)
    Mul hs ds    -> let es = map (flip justLookup m) hs
                    in add' (diffmul es)
    Fun sym hs   -> let es = map (flip justLookup m) hs
                        ies = zip [1..] es 
                    in add' (map (difff sym es) ies)
    Sum is h1    -> let MExp e1 _ _ = justLookup h1 m
                    in sum'_ is (untrie t (s,e1))
    Concat i hs  -> let es = map (flip justLookup m) hs
                        ies = zip [1..] es
                        iss = map (HS.toList . mexpIdx) es
                    in add' (map (diffc i iss) ies)
 where
  diffmul [] = []
  diffmul (x1:xs) = let x' = untrie t (s,mexpExp x1)
                        xs'all = diffmul xs
                    in (mul' (x':xs) : map (\y1 -> mul' [x1,y1]) xs'all)    
  -- 
  difff sym args (i,e1) = let e' = untrie t (s,mexpExp e1)
                          in mul' [fun (suffix_n i sym) args , e'] 

  -- 
  diffc i iss (n,e1) = let e' = untrie t (s,mexpExp e1)
                           si = iss !! (n-1)
                       in sum'_ si (mul' [cdelta i iss n,e']) 
                             

-- | differentiation of variables
dvar :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) =>
        DependencyMap -> Variable -> Variable -> MExp a
dvar dm (V x1 j) (V y1 k)
  | x1 == y1 && length j == length k = let djk = zipWith delta j k
                                       in mul' djk
  | otherwise =
    case (x1,y1) of 
      (Atom sx,Atom sy) ->
        let deps = maybe [] id (HM.lookup sy dm) 
        in if sx `elem` deps
           then let newsym = Deriv sy sx in ivar newsym (k++j)
           else zero
      _ -> zero -- for the time being 

-- | simple differentiation without complex memoization
sdiff :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) =>
         DependencyMap -> Variable -> MExp a -> MExp a
sdiff dm s (MExp e m _) = let diff = fix (diff' dm m . trie) in diff (s,e)

