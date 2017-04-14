{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.Print where

import           Control.Lens              (view,_1)
import           Control.Monad             ((>=>))
import           Control.Monad.Trans.State
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet              (HashSet)
import qualified Data.HashSet        as HS
import           Data.List                 (intercalate)
import           Data.MemoTrie
import           Text.Printf
--
import           Symbolic.Dependency
import           Symbolic.Type
-- 

listPrintf :: PrintfType r => String -> [String] -> r
listPrintf _   []     = printf ""
listPrintf _   (x:[]) = printf "%s" x
listPrintf sep (x:xs) = printf "%s%s%s" x sep (listPrintf sep xs :: String)

prettyPrintKD :: (PrintfType r) => KDelta -> r
prettyPrintKD (Delta i j) = printf "delta_%s%s" (indexName i) (indexName j)
prettyPrintKD (CDelta i iss p) = printf "cdelta_%s(%s)" (view _1 i) ((listPrintf "" . map (view _1)) (iss !! (p-1)) :: String)



prettyPrint :: (Show a, PrintfType r) => RExp a -> r
prettyPrint RZero = printf "0"
prettyPrint ROne  = printf "1"
prettyPrint (RVal n) = printf "%s" (show n) 
prettyPrint (RVar s) = printf "%s" (showVar s)
prettyPrint (RAdd es) = printf "(%s)" (listPrintf "+" (map prettyPrint es) :: String)
prettyPrint (RMul es ds) = printf "(%s)" (listPrintf "*" (map prettyPrint es++map prettyPrintKD ds) :: String)
prettyPrint (RFun s es) = printf "%s(%s)" s (listPrintf "," (map prettyPrint es) :: String)
prettyPrint (RSum is e1) = printf "sum_(%s) %s" (showIdxSet is) (prettyPrint e1 :: String)
prettyPrint (RConcat i es) = printf "(concat_(%s) (%s))" (showIdxSet [i]) (listPrintf "," (map prettyPrint es) :: String)


prettyPrintR :: (Show a) => MExp a -> IO ()
prettyPrintR = (prettyPrint . exp2RExp) >=> const endl

endl :: IO ()
endl = putStrLn ""

showIdxSet :: [Index] -> String
showIdxSet = intercalate "," . map (view _1)


-- dotPrint h (Delta i j)    = printf "x%x [label=\"delta_%s%s\"];\n" h (indexName i) (indexName j)
-- dotPrint h (CDelta i iss p) = printf "x%x [label=\"cdelta_%s(%s)\"];\n" h (indexName i) ((listPrintf "" . map indexName) (iss !! (p-1)) :: String)


dotPrint' :: (Show a) => Hash -> Exp a -> String
dotPrint' h Zero           = printf "x%x [label=\"0\"];\n" h
dotPrint' h One            = printf "x%x [label=\"1\"];\n" h
dotPrint' h (Val n)        = printf "x%x [label=\"%s\"];\n" h (show n)
dotPrint' h (Var s)        = printf "x%x [label=\"%s\"];\n" h (showVar s)
dotPrint' h (Add hs)       = printf "x%x [label=\"+\"];\n" h ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
dotPrint' h (Mul hs ds)    = printf "x%x [label=\"*%s\"];\n" h (listPrintf "*" (map prettyPrintKD ds) :: String)
                             ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
dotPrint' h (Fun s hs)     = printf "x%x [label=\"%s\"];\n" h s ++ (concatMap (printf "x%x -> x%x;\n" h) hs)
dotPrint' h (Sum is h1)    = printf "x%x [label=\"sum_(%s)\"];\nx%x -> x%x;\n" h (showIdxSet is) h h1
dotPrint' h (Concat i hs)  = printf "x%x [label=\"concat_(%s)\"];\n" h (showIdxSet [i]) ++ (concatMap (printf "x%x -> x%x;\n" h) hs)

-- |
dotPrint :: (Show a) => HashMap Hash (MExp a) -> Hash -> State (HashSet Hash) String
dotPrint m h = do
  s <- get
  let MExp e _ _ = justLookup h m
  case h `HS.member` s of
    True -> return ""
    False -> do
      let str = dotPrint' h e
          hs = daughters e
      put (h `HS.insert` s)
      lst <- mapM (dotPrint m) hs
      return (concat (str : lst))

digraph :: (HasTrie a, Show a, ?expHash :: Exp a :->: Hash) => MExp a -> IO ()
digraph v = do
    let h = untrie ?expHash (mexpExp v)
        m = HM.insert h v (mexpMap v) 
    putStrLn "digraph G {"
    putStrLn $ evalState (dotPrint m h) HS.empty
    putStrLn "}"

-- for debugging

debugExp s e = printf "%s = %s\n" s ((prettyPrintD . exp2RExp) e :: String)


prettyPrintD :: (PrintfType r) => RExp a -> r
prettyPrintD RZero = printf "0"
prettyPrintD ROne  = printf "1"
prettyPrintD (RVal n) = printf "%s" "somen"
prettyPrintD (RVar s) = printf "%s" (showVar s)
prettyPrintD (RAdd es) = printf "(%s)" (listPrintf "+" (map prettyPrintD es) :: String)
prettyPrintD (RMul es ds) = printf "(%s)" (listPrintf "*" (map prettyPrintD es++map prettyPrintKD ds) :: String)
prettyPrintD (RFun s es) = printf "%s(%s)" s (listPrintf "," (map prettyPrintD es) :: String)
prettyPrintD (RSum is e1) = printf "sum_(%s) %s" (showIdxSet is) (prettyPrintD e1 :: String)
prettyPrintD (RConcat i es) = printf "(concat_(%s) (%s))" (showIdxSet [i]) (listPrintf "," (map prettyPrintD es) :: String)
