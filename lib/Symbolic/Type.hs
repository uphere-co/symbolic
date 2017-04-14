{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Type where

import           Control.Lens              (over, view, _1)
import qualified Data.Binary as Bi
import qualified Data.ByteString.Lazy as LB
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.MemoTrie
import           Data.Vector.Storable      (Vector)
import           Text.Encoding.Z           (zEncodeString)
--

type Hash = Int

type IndexSymbol = String

type Index = (IndexSymbol,Int,Int)

indexName :: Index -> IndexSymbol
indexName = view _1

data Symbol = Atom String
            | Deriv String String
            deriving (Show, Eq)

instance HasTrie Symbol where
  data (Symbol :->: b) = SymbolTrie (String :->: b) ((String,String) :->: b)
  
  trie :: (Symbol -> b) -> (Symbol :->: b)
  trie f = SymbolTrie (trie (f . Atom)) (trie (f . uncurry Deriv))

  untrie :: (Symbol :->: b) -> Symbol -> b
  untrie (SymbolTrie a d) (Atom x) = untrie a x  
  untrie (SymbolTrie a d) (Deriv f x) = untrie d (f,x)

  enumerate :: (Symbol :->: b) -> [(Symbol,b)]
  enumerate (SymbolTrie a d) = enum' Atom a `weave` enum' (uncurry Deriv) d  

instance Hashable Symbol where
  hashWithSalt :: Hash -> Symbol -> Hash
  hashWithSalt s (Atom x)    = s `hashWithSalt` (0 :: Int) `hashWithSalt` x
  hashWithSalt s (Deriv f x) = s `hashWithSalt` (1 :: Int) `hashWithSalt` f `hashWithSalt` x
  
type DependencyMap = HashMap String [String]   -- dependency map between atomic symbols


showSym (Atom s) = s
showSym (Deriv f x) = ('d':f)++('/':'d':x) 

zencSym :: Symbol -> String
zencSym = zEncodeString . showSym


mkSym = Atom

data Variable = V Symbol [Index] deriving (Show, Eq)

varName :: Variable -> String
varName (V v _) = showSym v

showVar :: Variable -> String
showVar (V x k)
  | null k    = showSym x
  | otherwise = showSym x ++ "_" ++ concat (map indexName k)


instance HasTrie Variable where
  data (Variable :->: b) = VariableTrie ((Symbol,[Index]) :->: b)
  
  trie :: (Variable -> b) -> (Variable :->: b)
  trie f = VariableTrie (trie (f . uncurry V))

  untrie :: (Variable :->: b) -> Variable -> b
  untrie (VariableTrie i) (V x k) = untrie i (x,k)

  enumerate :: (Variable :->: b) -> [(Variable,b)]
  enumerate (VariableTrie i) = enum' (uncurry V) i  

instance Hashable Variable where
  hashWithSalt :: Hash -> Variable -> Hash
  hashWithSalt s (V x k) = s `hashWithSalt` x `hashWithSalt` k

data KDelta = Delta Index Index
            | CDelta Index [[Index]] Int
                 -- ^ delta for collective index
                 -- (collective index, index scheme, partition number (1-based))
            deriving (Show,Eq)


data Exp a = Zero
           | One
           | Val a
           | Var Variable
           | Add [Hash]
           | Mul [Hash] [KDelta]
           | Fun String [Hash]
           | Sum [Index] Hash
           | Concat Index [Hash]
         deriving (Show,Eq)

isZero :: Exp a -> Bool
isZero Zero = True
isZero _ = False

isOne :: Exp a -> Bool
isOne One = True
isOne _ = False

isSum :: Exp a -> Bool
isSum (Sum _ _) = True
isSum _         = False

isSumOrConcat :: Exp a -> Bool
isSumOrConcat (Sum _ _)    = True
isSumOrConcat (Concat _ _) = True
isSumOrConcat _            = False


deltaIndex :: KDelta -> [Index]
deltaIndex (Delta j k)      = [j,k]
deltaIndex (CDelta i iss p) = i:(iss !! (p-1))

data MExp a = MExp { mexpExp :: Exp a
                   , mexpMap :: HashMap Hash (MExp a)
                   , mexpIdx :: HashSet Index
                   }

getMHash :: (HasTrie a, ?expHash :: Exp a :->: Hash) => MExp a -> Hash
getMHash e = untrie ?expHash (mexpExp e)

data RExp a = RZero
            | ROne
            | RVal a
            | RVar Variable
            | RAdd [RExp a]
            | RMul [RExp a] [KDelta]
            | RFun String [RExp a]
            | RSum [Index] (RExp a)
            | RConcat Index [RExp a]

mangle :: Double -> [Int]
mangle = map fromIntegral . LB.unpack . Bi.encode

unmangle :: [Int] -> Double
unmangle = Bi.decode . LB.pack . map fromIntegral

fmangle :: Float -> [Int]
fmangle = map fromIntegral . LB.unpack . Bi.encode

funmangle :: [Int] -> Float
funmangle = Bi.decode . LB.pack . map fromIntegral


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c 

instance HasTrie Double where
  data Double :->: a = DoubleTrie ([Int] :->: a)
  trie f = DoubleTrie $ trie $ f . unmangle
  untrie (DoubleTrie t) = untrie t . mangle

  -- for the time being
  enumerate = error "enumerate of HasTrie instance of Double is undefined"

instance HasTrie Float where
  data Float :->: a = FloatTrie ([Int] :->: a)
  trie f = FloatTrie $ trie $ f . funmangle
  untrie (FloatTrie t) = untrie t . fmangle

  -- for the time being
  enumerate = error "enumerate of HasTrie instance of Float is undefined"


instance HasTrie KDelta where
  data (KDelta :->: b) = KDTrie ((Index,Index) :->: b) ((Index,[[Index]],Int) :->: b)

  trie :: (KDelta -> b) -> (KDelta :->: b)
  trie f = KDTrie (trie (f . uncurry Delta))
                  (trie (f . uncurry3 CDelta))

  untrie :: (KDelta :->: b) -> KDelta -> b
  untrie (KDTrie d cd) e =
    case e of
      Delta i j    -> untrie d (i,j)
      CDelta i is p -> untrie cd (i,is,p)      
    
  enumerate :: (KDelta :->: b) -> [(KDelta,b)]
  enumerate (KDTrie d cd) = enum' (uncurry Delta) d `weave` enum' (uncurry3 CDelta) cd

instance HasTrie a => HasTrie (Exp a) where
  data (Exp a :->: b) = ExpTrie (() :->: b)
                                (() :->: b)
                                (a :->: b)
                                (Variable :->: b)
                                ([Hash] :->: b)     -- ^ for Add
                                (([Hash],[KDelta]) :->: b)     -- ^ for Mul
                                ((String,[Hash]) :->: b) -- ^ for Fun
                                (([Index],Hash) :->: b)
                                ((Index,[Hash]) :->: b)
                        
  trie :: (Exp a -> b) -> (Exp a :->: b)
  trie f = ExpTrie (trie (\() -> f Zero))
                   (trie (\() -> f One))
                   (trie (f . Val))
                   (trie (f . Var))
                   (trie (f . Add))
                   (trie (f . uncurry Mul))
                   (trie (f . uncurry Fun))
                   (trie (f . uncurry Sum))
                   (trie (f . uncurry Concat))
           
  untrie :: (Exp a :->: b) -> Exp a -> b
  untrie (ExpTrie z o l v a m f su c) e =
    case e of
      Zero         -> untrie z  ()
      One          -> untrie o  ()
      Val n        -> untrie l  n
      Var s        -> untrie v  s
      Add hs       -> untrie a  hs
      Mul hs ds    -> untrie m  (hs,ds)
      Fun s hs     -> untrie f  (s,hs) 
      Sum is h     -> untrie su (is,h)
      Concat i hs  -> untrie c  (i,hs)
                                     
  enumerate :: (Exp a :->: b) -> [(Exp a,b)]
  enumerate (ExpTrie z o n v a m f su c) =
    enum' (\()->Zero) z
    `weave`
    enum' (\()->One) o
    `weave`
    enum' Val n
    `weave`
    enum' Var v
    `weave`
    enum' Add a
    `weave`
    enum' (uncurry Mul) m
    `weave`
    enum' (uncurry Fun) f
    `weave`
    enum' (uncurry Sum) su
    `weave`
    enum' (uncurry Concat) c

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a',b)]
enum' f = fmap (over _1 f) . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a:as) `weave` bs = a : (bs `weave` as)

instance Hashable KDelta where
  hashWithSalt s (Delta i j)     = s `hashWithSalt` (0 :: Int) `hashWithSalt` i `hashWithSalt` j
  hashWithSalt s (CDelta i iss p)= s `hashWithSalt` (1 :: Int) `hashWithSalt` i `hashWithSalt` iss `hashWithSalt` p
  
  
instance Hashable a => Hashable (Exp a) where
  hashWithSalt :: Hash -> Exp a -> Hash
  hashWithSalt s Zero            = s `hashWithSalt` (0 :: Int)
  hashWithSalt s One             = s `hashWithSalt` (1 :: Int)
  hashWithSalt s (Val n)         = s `hashWithSalt` (2 :: Int) `hashWithSalt` n
  hashWithSalt s (Var s')        = s `hashWithSalt` (3 :: Int) `hashWithSalt` s'
  hashWithSalt s (Add hs)        = s `hashWithSalt` (4 :: Int) `hashWithSalt` hs
  hashWithSalt s (Mul hs ds)     = s `hashWithSalt` (5 :: Int) `hashWithSalt` hs `hashWithSalt` ds
  hashWithSalt s (Fun s' hs)     = s `hashWithSalt` (6 :: Int) `hashWithSalt` s' `hashWithSalt` hs
  hashWithSalt s (Sum is h1)     = s `hashWithSalt` (7 :: Int) `hashWithSalt` is `hashWithSalt` h1
  hashWithSalt s (Concat i hs)   = s `hashWithSalt` (8 :: Int) `hashWithSalt` i `hashWithSalt` hs


-- exp2RExp (MExp (Delta i j) _ _) = RDelta i j
-- exp2RExp (MExp (CDelta i is j) _ _) = RCDelta i is j


exp2RExp :: MExp a -> RExp a
exp2RExp (MExp Zero        _ _) = RZero
exp2RExp (MExp One         _ _) = ROne
exp2RExp (MExp (Val n)     _ _) = RVal n
exp2RExp (MExp (Var s)     _ _) = RVar s
exp2RExp (MExp (Add hs)    m _) = RAdd $ map (exp2RExp . flip justLookup m) hs
exp2RExp (MExp (Mul hs ds) m _) = RMul (map (exp2RExp . flip justLookup m) hs) ds
exp2RExp (MExp (Fun s hs)  m _) = RFun s $ map (exp2RExp . flip justLookup m) hs
exp2RExp (MExp (Sum is h1) m _) = let e1 = justLookup h1 m in RSum is (exp2RExp e1)
exp2RExp (MExp (Concat i hs) m _) = RConcat i $ map (exp2RExp . flip justLookup m) hs

data Pos = Pos1 | Pos2 

type IdxPoint = [(IndexSymbol,Int)]

data Args a = Args { argMap :: HashMap Symbol (Vector a) }

type FunctionMap a = HashMap String ([a] -> a)


justLookup :: (Eq k, Hashable k,Show k) => k -> HashMap k v -> v
justLookup h m = case (HM.lookup h m) of
                   Nothing -> error ("justLookup: error in retrieving " ++ show h)   -- this is very unsafe, but we do not have good solution yet.
                   Just v -> v

justLookupL :: (Eq k,Show k,Show v) => k -> [(k,v)] -> v
justLookupL k l = case (lookup k l) of
                    Nothing -> error ("justLookup: error in retrieving " ++ show k ++ " in " ++ show l)
                    Just v -> v



