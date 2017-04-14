{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Concurrent
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Foldable                  ( forM_ )
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.MemoTrie
import qualified Data.Vector.Storable  as VS
import           Foreign.ForeignPtr             ( withForeignPtr )
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Marshal.Array as Array
import           Foreign.Ptr                    ( Ptr )
import           Foreign.Storable               ( poke, peek, pokeElemOff )

import qualified LLVM.General.AST          as AST
import           LLVM.General.AST.Type            ( double, float, ptr, void )
import           LLVM.General.Context             ( withContext )
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--
import           Test.LLVM

testexp1 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp1 = mul [val 1,val 3]

testexp2 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp2 = power 10 varx

testexp3 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp3 = delta idxi idxj 
  where idxi = ("i",0,2)
        idxj = ("j",0,2)

testexp4 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp4 = add [ zero , y_ [("i",0,3),("j",1,2)] ] 


testexp6 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp6 = sum_ [("i",0,9)] (fun "sin" [ y_ [("i",0,9)] ])

testexp7 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp7 = add [ varx, vary ] 

testexp8 :: (HasTrie a, Num a, ?expHash :: Exp a :->: Hash) => MExp a
testexp8 = sum_ [idxj] (mul [ x_ [idxi,idxj] , y_ [ idxj ] ] )
  where idxi = ("i",0,9)
        idxj = ("j",0,9)

test2 :: LLVMContextT IO (Either String ())
test2 = do
  let ?expHash = trie hash
  let exp1 :: MExp Float
      exp1 = sum_ [idxi, idxj] (y_ [idxi,idxj])
      idxi = ("i",0,2)
      idxj = ("j",0,2)
  
  liftIO $ prettyPrintR exp1
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ V (mkSym "y") [idxi, idxj] ] exp1
              external double "sin" [(double, AST.Name "x")] 
             
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), xref ]
                ret_
  runJIT "main" ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        Alloc.alloca $ \pres -> 
          Alloc.alloca $ \pargs -> 
            Array.withArray [100,200,300,400,500,600,700,800,900,1000] $ \px -> do
              poke pargs px
              run fn pres pargs
              res <- peek pres
              putStrLn $ "Evaluated to: " ++ show res
          
test3 :: LLVMContextT IO (Either String ())
test3 = do
  let ?expHash = trie hash
  liftIO $ prettyPrintR testexp7
  let ast = runLLVM initModule $ do
              llvmAST "fun1" [ V (mkSym "x") [], V (mkSym "y") [] ] testexp7
              external double "sin" [(double, AST.Name "x")] 
             
              define void "main" [ (ptr double, AST.Name "res")
                                 , (ptr (ptr double), AST.Name "args")
                                 ] $ do
                xref <- getElem (ptr double) "args" (ival 0)
                yref <- getElem (ptr double) "args" (ival 1)
                call (externf (AST.Name "fun1")) [ local (AST.Name "res"), xref, yref ]
                ret_
  runJIT "main" ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        forkOS $ do
          threadDelay 500000
          Alloc.alloca $ \pres -> 
            Alloc.alloca $ \pargs -> 
              Alloc.alloca $ \px ->
                Alloc.alloca $ \py -> do
                  poke px 700
                  poke py 300
                  pokeElemOff pargs 0 px
                  pokeElemOff pargs 1 py
                  run fn pres pargs
                  res <- peek pres
                  putStrLn $ "Evaluated to: " ++ show res
        do
          threadDelay 1000000
          Alloc.alloca $ \pres -> 
            Alloc.alloca $ \pargs -> 
              Alloc.alloca $ \px ->
                Alloc.alloca $ \py -> do
                  poke px 500
                  poke py 900
                  pokeElemOff pargs 0 px
                  pokeElemOff pargs 1 py
                  run fn pres pargs
                  res <- peek pres
                  putStrLn $ "Evaluated to: " ++ show res


test4 :: LLVMContextT IO (Either String ())
test4 = do
  let ?expHash = trie hash
  liftIO $ prettyPrintR (testexp3 :: MExp Float)
  let ast = mkAST testexp3 []
  runJIT "fun1" ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        Array.allocaArray 9 $ \pres -> 
          Alloc.alloca $ \pargs -> do
            run fn pres pargs
            res <- Array.peekArray 9 pres
            putStrLn $ "Evaluated to: " ++ show res

test5 :: LLVMContextT IO (Either String ())
test5 = do
  let ?expHash = trie hash
  liftIO $ prettyPrintR (testexp8 :: MExp Float)
  let idxi = ("i",1,10)
      idxj = ("j",1,10)
  let ast = mkAST testexp8  [ V (mkSym "x") [idxi,idxj], V (mkSym "y") [idxj] ]
      vx = VS.fromList [1..100] :: VS.Vector Float
      vy = VS.fromList [1..10]  :: VS.Vector Float
      vr = VS.replicate 10 0    :: VS.Vector Float
  runJITASTPrinter "fun1" (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr

test6 :: LLVMContextT IO (Either String ())
test6 = do
  let ?expHash = trie hash
  let exp1 :: MExp Float
      exp1 = concat_ idxA [ x_ [idxi,idxj], y_ [idxk] ] 
      idxA = ("A",1,10)
      idxi = ("i",1,2)
      idxj = ("j",1,3)
      idxk = ("k",1,4)
  liftIO $ prettyPrintR (exp1 :: MExp Float)
  -- digraph exp
  let ast = mkAST exp1 [ V (mkSym "x") [idxi,idxj], V (mkSym "y") [idxk] ]
      vx  = VS.fromList [1,2,3,4,5,6]
      vy  = VS.fromList [11,12,13,14]  :: VS.Vector Float
      vr  = VS.replicate 10 0    :: VS.Vector Float
  runJITASTPrinter "fun1" (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr

test7 :: LLVMContextT IO ()
test7 = do
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxI = ("I",1,4)
      exp1 :: MExp Float
      exp1 = concat_ idxI [ x_ [idxi], y_ [idxj] ]
  let exp2 :: MExp Float
      exp2 = mul [ cdelta idxI [[idxi],[idxj]] 2, exp1 ] 
  liftIO $ prettyPrintR exp2
  -- digraph exp2
  let ast = mkAST exp2 [ V (mkSym "x") [idxi], V (mkSym "y") [idxj] ]
      vx = VS.fromList [101,102]
      vy = VS.fromList [203,204] :: VS.Vector Float
      vr = VS.replicate 8 0    :: VS.Vector Float
  runJITASTPrinter "fun1" (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr

  -- comparison
  let xvals = VS.fromList [101,102]
      yvals = VS.fromList [203,204]
      args = Args (HM.fromList [(mkSym "x",xvals),(mkSym "y",yvals)])
  forM_ [(i,j) | i <- [1,2,3,4], j <- [1,2] ] $ \(i,j) -> do
    let iptI = [("I",i)]
        iptj = [("j",j)]
        
    liftIO $ printf "val(I=%d,j=%d) = %f \n" i j (seval args (iptI++iptj) exp2)
  

test8 :: LLVMContextT IO ()
test8 = do
  let idxi = ("i",1,2)
      idxj = ("j",1,2)

      idxI = ("I",1,4)
      idxk = ("k",1,2)
  
  let ?expHash = trie hash
      ?functionMap = HM.empty
  let exp1 :: MExp Float
      exp1 = concat_ idxI [ mul [ x_ [idxi], x_ [idxi] ]  , mul [ y_ [idxj], x_ [idxj] ] ]
      dm = HM.fromList [ ("y", ["x"]) ]
      exp' = sdiff dm (V (mkSym "x") [idxk]) exp1
  liftIO $ do
    putStr "f = "
    prettyPrintR exp1
    putStr "df/dx_k = "
    prettyPrintR exp'
  let ast = mkAST exp' [ V (mkSym "x") [idxi]
                       , V (mkSym "y") [idxj]
                       , V (Deriv "y" "x") [idxj,idxi]
                       ]
      vx = VS.fromList [101,102]
      vy = VS.fromList [203,204] :: VS.Vector Float
      vdydx = VS.fromList [0,1,1,0] 
      vr = VS.replicate 8 0    :: VS.Vector Float
  liftIO $ do
    putStrLn "====================="
    putStrLn "=    LLVM result    ="
    putStrLn "====================="
  runJITASTPrinter "fun1" (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy,vdydx] vr
  liftIO $ do
    putStrLn "======================"
    putStrLn "= interpreter result ="
    putStrLn "======================"
  let  args = Args (HM.fromList [(mkSym "x",vx)
                                ,(mkSym "y",vy)
                                ,(Deriv "y" "x",vdydx)
                                ])
  
  forM_ [(iI,k) | iI <- [1,2,3,4], k <- [1,2] ] $ \(iI,k) -> do
    let iptI = [("I",iI)]
        iptk = [("k",k)]
    liftIO $ printf "val(I=%d,k=%d) = %f \n" iI k (seval args (iptI++iptk) exp')


test9 :: LLVMContextT IO ()
test9 = do
  let idxi = ("i",1,2)
      idxj = ("j",1,2)
      idxI = ("I",1,4)
  
  let ?expHash = trie hash
      ?functionMap = HM.fromList [("temp", (/100.0) . head)]      
  let exp1 :: MExp Float
      exp1 = fun "temp" [concat_ idxI [ mul [ x_ [idxi], x_ [idxi] ]  , mul [ y_ [idxj], x_ [idxj] ] ] ]
  liftIO $ do
    putStr "f = "
    prettyPrintR exp1
  let ext = define float "temp" [(float, AST.Name "x")] $ do
              let xref = local (AST.Name "x")
              v <- fdiv xref (fval 100)
              ret v
      ast = mkASTWithExt ext [("fun1",(exp1,[ V (mkSym "x") [idxi], V (mkSym "y") [idxj] ]))]
      vx = VS.fromList [101,102]
      vy = VS.fromList [203,204] :: VS.Vector Float
      vr = VS.replicate 4 0    :: VS.Vector Float
  liftIO $ do
    putStrLn "====================="
    putStrLn "=    LLVM result    ="
    putStrLn "====================="
  runJITASTPrinter "fun1" (\r->putStrLn $ "Evaluated to: " ++ show r) ast [vx,vy] vr
  liftIO $ do
    putStrLn "======================"
    putStrLn "= interpreter result ="
    putStrLn "======================"
  let  args = Args (HM.fromList [(mkSym "x",vx)
                                ,(mkSym "y",vy)
                                ])
  
  forM_ [ iI | iI <- [1,2,3,4] ] $ \iI -> do
    let iptI = [("I",iI)]
    liftIO $ printf "val(I=%d) = %f \n" iI (seval args iptI exp1)


main = withContext $ \context -> flip runReaderT context delta_test

