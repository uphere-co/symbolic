{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Symbolic.CodeGen.C where

import           Data.Array                      ((!))
import           Data.Graph                      (topSort)
import qualified Data.HashMap.Strict       as HM
import           Data.List                       (foldl')
import           Data.MemoTrie
import           Language.C.Data
import           Language.C.Data.Ident
import           Language.C.Data.Position
import           Language.C.Pretty
import           Language.C.Syntax
import           Text.Printf
import           Text.PrettyPrint
--
import           Symbolic.Dependency
import           Symbolic.Type
--


pos :: Position
pos = position 0 "test" 0 0

nodeinfo :: NodeInfo
nodeinfo = OnlyPos pos (pos,0)

ident :: String -> Ident
ident name = Ident name 0 nodeinfo

ptr :: CDerivedDeclr
ptr = CPtrDeclr [] nodeinfo

data Const = IConst Int
           | FConst Double

mkI :: Int -> Const
mkI = IConst

mkF :: Double -> Const
mkF = FConst

mkConst :: Const -> CExpr
mkConst (IConst i) = CConst (CIntConst (cInteger (fromIntegral i)) nodeinfo)
mkConst (FConst f) = CConst (CFloatConst (readCFloat (show f)) nodeinfo)

mkDecl :: (NodeInfo -> CTypeSpec) -> Int -> String -> Maybe Const -> CDecl
mkDecl typ ptrnum name mv =
  let cdeclr = CDeclr (Just (ident name)) (replicate ptrnum ptr) Nothing [] nodeinfo
      typespec = CTypeSpec (typ nodeinfo)
      mv' = fmap (\v -> CInitExpr (mkConst v) nodeinfo) mv
  in CDecl [typespec] [(Just cdeclr,mv',Nothing)] nodeinfo

mkDblVarDecl :: String -> CDecl
mkDblVarDecl str = mkDecl CDoubleType 0 str (Just (mkF 0))

mkCFunction :: (NodeInfo -> CTypeSpec) -> String -> [CDecl] -> [CBlockItem] -> CFunDef
mkCFunction typ name decllst bodylst =
  let typspec  = CTypeSpec (typ nodeinfo)
      fun = CFunDeclr (Right (decllst,False)) [] nodeinfo
      cdeclr = CDeclr (Just (ident name)) [fun] Nothing [] nodeinfo
      ccompound = CCompound [] bodylst  nodeinfo
  in CFunDef [typspec] cdeclr [] ccompound nodeinfo

mkArgs :: [Variable] -> [CDecl]
mkArgs = map mkArg
 where mkArg (V s is) = mkDecl CDoubleType (length is) (zencSym s) Nothing

mkFor :: String -> Int -> Int -> CStat -> CStat
mkFor name start end stmts =
 CFor (Right (mkDecl CIntType 0 name (Just (mkI start))))
      (Just (mkBinary (mkVar name) CLeqOp (mkConst (mkI end))))
      (Just (mkUnary name CPostIncOp))
      stmts nodeinfo

mkVar :: String -> CExpr
mkVar name = CVar (ident name) nodeinfo

mkIVar :: (Foldable t) => Symbol -> t IndexSymbol -> CExpr
mkIVar name is = foldl' (\acc i -> CIndex acc (mkVar i) nodeinfo) (mkVar (zencSym name)) is

mkUnary :: String -> CUnaryOp -> CExpr
mkUnary name op = CUnary op (mkVar name) nodeinfo

mkBinary :: CExpr -> CBinaryOp -> CExpr -> CExpr
mkBinary x op y = CBinary op x y nodeinfo

mkAssign :: String -> CExpr -> CExpr
mkAssign name value = CAssign CAssignOp (mkVar name) value nodeinfo

mkAssignAdd :: String -> CExpr -> CExpr
mkAssignAdd name value = CAssign CAddAssOp (mkVar name) value nodeinfo

mkExpr :: CExpr -> CStat
mkExpr e = CExpr (Just e) nodeinfo

mkCompound :: [CBlockItem] -> CStat
mkCompound stmts = CCompound [] stmts nodeinfo 

mkCall :: String -> [CExpr] -> CExpr
mkCall sym lst = CCall (mkVar sym) lst nodeinfo

mkReturn :: CExpr -> CStat
mkReturn e = CReturn (Just e) nodeinfo

hVar :: Int -> String
hVar h = printf "x%x" h

cPrint4KDelta :: String -> KDelta -> [CStat]
cPrint4KDelta name (Delta i j) = [ CIf cond  stru (Just sfal) nodeinfo ]
  where cond = mkBinary (mkVar (indexName i)) CEqOp (mkVar (indexName j))
        stru = mkExpr (mkAssign name (mkConst (mkI 1)))
        sfal = mkExpr (mkAssign name (mkConst (mkI 0)))
cPrint4KDelta name (CDelta _ _ _) = error "cPrint4KDelta undefined for CDelta case"        

cPrint' :: (?expHash :: Exp Double :->: Hash)=> String -> MExp Double -> [CStat] 
cPrint' name (MExp Zero        _ _)  = [ mkExpr (mkAssign name (mkConst (mkI 0))) ]
cPrint' name (MExp One         _ _)  = [ mkExpr (mkAssign name (mkConst (mkI 1))) ]
cPrint' name (MExp (Var v)     _ _)  = [ mkExpr (mkAssign name rhs) ] 
  where rhs = case v of
                V s is -> mkIVar s (map indexName is)
cPrint' name (MExp (Val n)     _ _)  = [ mkExpr (mkAssign name (mkConst (mkF n))) ]
cPrint' name (MExp (Add hs)    _ _)  = [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CAddOp)) lst ]
  where lst = map (mkVar . hVar) hs
cPrint' name (MExp (Mul hs ds)    _ _)  = [ (mkExpr . mkAssign name . foldr1 (flip mkBinary CMulOp)) lst ]
  where lst = map (mkVar . hVar) hs
cPrint' name (MExp (Fun sym hs) _ _) = [ mkExpr (mkAssign name (mkCall sym lst)) ]
  where lst = map (mkVar . hVar) hs
cPrint' name (MExp (Sum is h1)  m _) = [ mkExpr (mkAssign name (mkConst (mkF 0)))
                                          , foldr (.) id (map (uncurry3 mkFor) is) innerstmt ]
  where v = justLookup h1 m
        h_result = untrie ?expHash (mexpExp v)
        (_hashmap,table,depgraph) = mkDepGraphNoSum v
        bmap = HM.insert h_result v (mexpMap v)
        hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
        es_ordered = map (flip justLookup bmap) hs_ordered
        decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
        bodylst' = map CBlockStmt . concatMap (\e -> cPrint' (hVar (getMHash e)) e) $ es_ordered
        innerstmt =
          mkCompound $  
            decllst ++ bodylst' ++ [CBlockStmt (mkExpr (mkAssignAdd name (mkVar (hVar h1))))]
cPrint' _name (MExp (Concat _i _hs)  _m _) = error "cPrint': Concat not implemented"

        
cAST :: (?expHash :: Exp Double :->: Hash) => String -> [Variable] -> MExp Double -> CTranslUnit
cAST name syms v = 
  let h_result = untrie ?expHash (mexpExp v)
      (_hashmap,table,depgraph) = mkDepGraphNoSum v
      bmap = HM.insert h_result v (mexpMap v)
      hs_ordered = reverse (map (\i -> table ! i) (topSort depgraph))
      es_ordered = map (flip justLookup bmap) hs_ordered
      decllst = map (CBlockDecl . mkDblVarDecl . hVar . getMHash) es_ordered
      bodylst' = map CBlockStmt . concatMap (\e -> cPrint' (hVar (getMHash e)) e) $ es_ordered
      bodylst = decllst ++ bodylst' ++ [CBlockStmt (mkReturn (mkVar (hVar h_result))) ]
  in CTranslUnit [CFDefExt (mkCFunction CDoubleType name (mkArgs syms) bodylst)] nodeinfo

cPrint :: (?expHash :: Exp Double :->: Hash) =>
          String -> [Variable] -> MExp Double -> IO ()
cPrint name syms v = let ctu = cAST name syms v in (putStrLn . render . pretty) ctu


