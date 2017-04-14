{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.Predefined where

import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet, difference)
import qualified Data.HashSet        as HS
import           Data.List                 (foldl')
import           Data.MemoTrie
import           Data.Monoid               ((<>))
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable as V
--
import           Symbolic.Type
--

noStructure :: Exp a -> MExp a
noStructure e = MExp e HM.empty HS.empty

tV :: [Float] -> Vector Float
tV = V.fromList

mkV :: (String,[Index]) -> Variable
mkV (s,is) = V (mkSym s) is

embedVar :: Variable -> MExp a
embedVar v@(V _ is) = MExp (Var v) HM.empty (HS.fromList is)

var :: Symbol -> MExp a
var s = noStructure (Var (V s []))

iV :: (String,[Index]) -> MExp a
iV (s,is) = ivar (mkSym s) is

ivar :: Symbol -> [Index] -> MExp a
ivar n i = MExp (Var (V n i)) HM.empty (HS.fromList i)

varx :: MExp a
varx = var (mkSym "x")

vary :: MExp a
vary = var (mkSym "y")

varz :: MExp a
varz = var (mkSym "z")

x_ :: [Index] -> MExp a
x_ i = ivar (mkSym "x") i

y_ :: [Index] -> MExp a
y_ i = ivar (mkSym "y") i

z_ :: [Index] -> MExp a
z_ i = ivar (mkSym "z") i

one :: MExp a
one = noStructure One

zero :: MExp a 
zero = noStructure Zero

val :: a -> MExp a
val n = noStructure (Val n)


delta :: Index -> Index -> MExp a
delta j k = MExp (Mul [] [d]) HM.empty (HS.fromList (deltaIndex d))
  where d = Delta j k
        
cdelta :: Index -> [[Index]] -> Int -> MExp a
cdelta i iss p = MExp (Mul [] [cd]) HM.empty (HS.fromList (deltaIndex cd))
  where cd = (CDelta i iss p)

kdelta :: KDelta -> MExp aa
kdelta (Delta i j)      = delta i j
kdelta (CDelta i iss p) = cdelta i iss p  



varop :: (HasTrie a, ?expHash :: Exp a :->: Hash) => ([Hash] -> Exp a) -> [MExp a] -> MExp a
varop op es = let (hs,m,is) = findTriple es
              in MExp (op hs) m is

findTriple :: (HasTrie a, ?expHash :: Exp a :->: Hash) =>
              [MExp a] -> ([Hash],HashMap Hash (MExp a), HashSet Index)
findTriple es = let hes = map ((,) <$> getMHash <*> id) es
                    hs = map fst hes
                    ms = map mexpMap es
                    is = map mexpIdx es
                    m' = foldl' HM.union HM.empty ms
                    i' = foldl' HS.union HS.empty is
                    m'' = foldr (uncurry HM.insert) m' hes 
                in (hs, m'', i')

add :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
add = varop Add

mul :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
mul es = let (hs,m,is) = findTriple es
         in MExp (Mul hs []) m is

sum_ :: (HasTrie a, ?expHash :: Exp a  :->: Hash) => [Index] -> MExp a -> MExp a
sum_ is em@(MExp e1 m1 i1) =
  let h1 = untrie ?expHash e1
      i = i1 `difference` HS.fromList is
      e = Sum is h1
      m = HM.insert h1 em m1
  in MExp e m i

fun :: (HasTrie a, ?expHash :: Exp a :->: Hash) => String -> [MExp a] -> MExp a
fun sym = varop (Fun sym)

concat_ :: (HasTrie a, ?expHash :: Exp a :->: Hash) => Index -> [MExp a] -> MExp a
concat_ i es = MExp (Concat i hs) m'' (HS.singleton i)
  where hes = map ((,) <$> getMHash <*> id) es
        hs = map fst hes
        ms =map mexpMap es
        m' = foldl1 HM.union ms
        m'' = foldr (uncurry HM.insert) m' hes

square :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> MExp a
square e = mul [e,e] 

power :: (HasTrie a, ?expHash :: Exp a :->: Hash) => Int -> MExp a -> MExp a
power n e
  | n < 0          = error "not supported"
  | n == 1         = e
  | n == 0         = one
  | n `mod` 2 == 0 = square (power (n `div` 2) e)
  | otherwise      = mul [square (power (n `div` 2) e), e]


suffix' :: String -> String
suffix' = (<> "'")

suffix_1 :: String -> String
suffix_1 = (<> "_1")

suffix_2 :: String -> String
suffix_2 = (<> "_2")

suffix_n :: Int -> String -> String
suffix_n n = (<> ("_" ++ show n))

tanh_ :: (HasTrie a, ?expHash :: Exp a :->: Hash) => [MExp a] -> MExp a
tanh_ = fun "tanh"


