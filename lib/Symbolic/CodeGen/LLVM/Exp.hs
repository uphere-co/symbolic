{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.LLVM.Exp where

import           Control.Monad.State
import           Data.Array                               ((!))
import           Data.Foldable                            (foldrM)
import           Data.Function                            (on)
import           Data.Graph                               (topSort)
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashSet                      as HS
import           Data.List
import           Data.MemoTrie
import           LLVM.General.AST ( Operand(..) )
import qualified LLVM.General.AST                  as AST
import qualified LLVM.General.AST.IntegerPredicate as IP
import           LLVM.General.AST.Type                    (float, i64, ptr)
import qualified LLVM.General.AST.Type             as T   (void) 
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Dependency
import           Symbolic.Type
import qualified Symbolic.Type                     as S   (Exp(..))
import           Symbolic.Util                            (indexFlatteningFactors, sizeIndex)
--

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ q [] = return [q]
scanM f q (x:xs) = do
  q2 <- f q x
  qs <- scanM f q2 xs
  return (q:qs)

hVar :: Int -> String
hVar h = printf "x%x" h

mkOp :: (Operand -> Operand -> Codegen Operand) -> Int -> Operand -> Codegen Operand
mkOp op h val = getvar (hVar h) >>= op val

mkAdd :: Int -> Operand -> Codegen Operand
mkAdd = mkOp fadd

mkMul :: Int -> Operand -> Codegen Operand
mkMul = mkOp fmul

getElem :: AST.Type -> String -> Operand -> Codegen Operand
getElem ty s i = 
  let arr = LocalReference (ptr ty) (AST.Name s)
  in load =<< getElementPtr arr [i]

loadIndex :: Index -> Codegen Operand
loadIndex = load <=< getvar . indexName 

getIndex :: Index -> Codegen Operand
getIndex = getvar . indexName 

index0baseM :: Index -> Operand -> Codegen Operand
index0baseM (_,s,_) x = if s == 0 then return x else isub x (ival s)

renormalizeIndexM :: Index -> Operand -> Codegen Operand
renormalizeIndexM (_,s,_) x = if s == 0 then return x else iadd x (ival s)

flattenByM :: [Operand] -> [Int] -> Codegen Operand
flattenByM [] _ = return (ival 0)
flattenByM is fac = do
  (i1:irest) <-
    zipWithM (\x y -> if y == 1 then return x else imul x (ival y)) is fac 
  foldrM iadd i1 irest       

splitByM :: Operand -> [Int] -> Codegen [Operand]
splitByM j fac = (map fst . tail) <$> (scanM f (ival 0,j) fac)
  where f (_,m) i | i == 1    = return (m,ival 0)
                  | otherwise = (,) <$> udiv m (ival i) <*> urem m (ival i)

-- | Get a flat index from multi-dimensional indices. We assume that input indices
--   are always 0-base normalized.
flatIndexM :: [Index] -> [Operand] -> Codegen Operand
flatIndexM is ivs =   flattenByM ivs (indexFlatteningFactors is)

-- | Split a flat index to original multi-dimensional indices (but 0-base normalized).
splitIndexM :: [Index] -> Operand -> Codegen [Operand]
splitIndexM is j = splitByM j (indexFlatteningFactors is) 

splitIndexDisjointFM :: (MExp Float -> Codegen Operand)
                     -> [MExp Float]
                     -> Operand
                     -> Codegen Operand
splitIndexDisjointFM f lst jj = do
    case lst of
      []     -> error "splitIndexDisjointFM: empty list"
      [e]    -> f' e ((HS.toList . mexpIdx) e)
      (e:es) ->
        let (is:_iss) = map (HS.toList . mexpIdx) (e:es)
            label = concatMap indexName is
            size = ival (sizeIndex is)
        in cgencond float label (icmp IP.ULT jj size)
             (f' e is) (splitIndexDisjointFM f es =<< isub jj size)
  where
    f' e is = do
      js <- splitIndexM is jj
      zipWithM (\i j -> assign (indexName i) j) is js
      f e


cgen4fold :: (Int -> Operand -> Codegen Operand) -> Operand -> [Int] -> Codegen Operand
cgen4fold _  ini []     = return ini-- assign name (fval ini)
cgen4fold op _   (h:hs) = getvar (hVar h) >>= \val1 -> foldrM op val1 hs
  -- assign name v'

cgencond :: AST.Type
         -> String
         -> Codegen Operand
         -> Codegen Operand -> Codegen Operand
         -> Codegen Operand
cgencond typ label cond tr fl = do
  ifthen <- addBlock (label ++ ".then")
  ifelse <- addBlock (label ++ ".else")
  ifexit <- addBlock (label ++ ".exit")
  --
  condval <- cond
  cbr condval ifthen ifelse
  --
  setBlock ifthen
  trval <- tr
  br ifexit
  ifthen' <- getBlock
  --
  setBlock ifelse
  flval <- fl
  br ifexit
  ifelse' <- getBlock
  --
  setBlock ifexit
  phi typ [(trval,ifthen'), (flval,ifelse')]

cgenfor :: String -> Index -> Codegen () -> Codegen ()
cgenfor label (ivar,start,end) body = do
  forloop <- addBlock (label ++ ".loop")
  forexit <- addBlock (label ++ ".exit")
  --
  iref <- alloca i64
  store iref (ival start) 
  br forloop
  --
  setBlock forloop
  i <- load iref
  assign ivar i  
  body
  i' <- iadd i (ival 1)
  store iref i'
  --
  test <- icmp IP.ULE i' (ival end)
  cbr test forloop forexit
  --
  setBlock forexit
  return ()  

mkInnerbody :: (?expHash :: Exp Float :->: Hash) => MExp Float -> Codegen Operand
mkInnerbody v = do
  mapM_ (\e -> llvmCodegen (hVar (getMHash e)) e) $ es_ordered
  llvmCodegen (hVar h_result) v
 where 
  h_result = getMHash v
  bmap = HM.insert h_result v (mexpMap v)
  (_hashmap,table,depgraph) = mkDepGraphNoSum v
  hs_ordered = delete h_result (reverse (map (\i -> table ! i) (topSort depgraph)))
  es_ordered = map (flip justLookup bmap) hs_ordered

cgenKDelta :: KDelta -> Codegen Operand
cgenKDelta (Delta idxi idxj) = do
  let ni = indexName idxi
      nj = indexName idxj
  i <- getIndex idxi
  j <- getIndex idxj
  cgencond float ("delta"++ni++nj) (icmp IP.EQ i j) (return fone) (return fzero)
cgenKDelta (CDelta idxI iss p) = do
  let js = iss !! (p-1) 
      prejs = take (p-1) iss
      startjs = sum (map sizeIndex prejs)
      nI = indexName idxI
      nj = concatMap indexName js
  iI <- flatIndexM [idxI] =<< mapM getIndex [idxI]
  j0 <- flatIndexM js =<< mapM getIndex js
  j <- iadd (ival startjs) j0
  cgencond float ("cdelta"++nI++nj) (icmp IP.EQ iI j) (return fone) (return fzero)
  
llvmCodegen :: (?expHash :: Exp Float :->: Hash) =>
               String -> MExp Float -> Codegen Operand
llvmCodegen name (MExp Zero _ _)                 = assign name (fval 0)
llvmCodegen name (MExp One _ _)                  = assign name (fval 1)
llvmCodegen name (MExp (Var (V s is)) _ _) = 
   mapM getIndex is >>= flatIndexM is >>= getElem float (zencSym s) >>= assign name
llvmCodegen name (MExp (Val n) _ _)              = assign name (fval n)
llvmCodegen name (MExp (S.Add hs) _ _)           = cgen4fold mkAdd (fval 0) hs >>= assign name
llvmCodegen name (MExp (S.Mul hs ds) _ _)        = do
  d <- foldrM fmul (fval 1) =<< mapM cgenKDelta ds
  e <- cgen4fold mkMul (fval 1) hs
  f <- fmul d e 
  assign name f
llvmCodegen name (MExp (Fun sym hs) _ _)         = do
  lst <- mapM (getvar . hVar) hs
  val <- call (externf (AST.Name sym)) lst
  assign name val
llvmCodegen name (MExp (Sum is h1) m _)          = do
  sumref <- alloca float
  store sumref (fval 0)
  let mkFor = \(i,s,e) -> cgenfor ("for_" ++ i) (i,0,e-s)
      innerstmt = do
        mkInnerbody (justLookup h1 m)
        s <- load sumref
        v <- getvar (hVar h1)
        s' <- fadd s v 
        store sumref s'
        return ()
  foldr (.) id (map mkFor is) innerstmt
  rval <- load sumref
  assign name rval
llvmCodegen name (MExp (Concat i hs) m _is)    = do
  iI <- flatIndexM [i] =<< mapM getIndex [i]
  let es = map (flip justLookup m) hs
  r <- (\a -> splitIndexDisjointFM a es iI) $ \e -> do
    mkInnerbody e
    let innername = hVar (getMHash e)
    v <- getvar innername
    return v
  assign name r

-- | generate LLVM AST from a symbolic expression
llvmAST :: (?expHash :: Exp Float :->: Hash) =>
           String -> [Variable] -> MExp Float -> LLVM ()
llvmAST name syms v =
  define T.void name symsllvm $ do
    let rref = LocalReference (ptr float) (AST.Name "result")
        is = (sortBy (compare `on` (\(i,_,_)->i)) . HS.toList  . mexpIdx) v
    if null is
      then do
        mkInnerbody v
        val <- getvar (hVar (getMHash v))
        store rref val
        ret_
      else do
        let mkFor = \(i,s,e) -> cgenfor ("for_" ++ i) (i,0,e-s)
            innerstmt = do
              theindex <- flatIndexM is =<< mapM getIndex is
              mkInnerbody v
              val <- getvar (hVar (getMHash v))
              p <- getElementPtr rref [theindex]
              store p val
              return ()
        foldr (.) id (map mkFor is) innerstmt
        ret_
  where
    mkarg (V n _) = (ptr float,AST.Name (zencSym n))
    symsllvm = (ptr float, AST.Name "result") : (map mkarg syms)
