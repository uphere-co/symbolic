{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}

module Symbolic.CodeGen.LLVM.Run where

import           Control.Monad.IO.Class           ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class       ( MonadReader(ask) )
import           Control.Monad.Trans.Reader       ( ReaderT(..) )
import           Data.MemoTrie                    ( (:->:) )
import qualified Data.Vector.Storable      as VS
import           Foreign.ForeignPtr               ( ForeignPtr, withForeignPtr )
import           Foreign.Ptr                      ( Ptr )
import qualified LLVM.General.AST          as AST
import           LLVM.General.AST.Type            ( double, float, ptr, void )
import           LLVM.General.Context             ( withContext )
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module          as Mod
import           Text.Encoding.Z                     (zEncodeString)
--
import           Symbolic.CodeGen.LLVM.Exp
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.Type

initModule :: AST.Module
initModule = emptyModule "my cool jit"

mkArgRef :: Int -> a -> Codegen AST.Operand
mkArgRef i _ = getElem (ptr float) "args" (ival i)

mkASTWithExt :: (?expHash :: Exp Float :->: Hash) =>
                LLVM ()
             -> [(String,(MExp Float,[Variable]))]
             -> AST.Module
mkASTWithExt external lst =
  runLLVM initModule $ do
    external
    mapM_ (\(n,(e,vs)) -> mkAST1 n e vs) lst

mkAST1 :: (?expHash :: Exp Float :->: Hash) =>
          String -> MExp Float -> [Variable] -> LLVM ()
mkAST1 n e args = do
  let zn = zEncodeString n
  llvmAST (zn++"i") args e
  define void zn [ (ptr float, AST.Name "res")
                                , (ptr (ptr float), AST.Name "args") ] $ do
    argrefs <- mapM (uncurry mkArgRef) (zip [0..] args)
    call (externf (AST.Name (zn++"i"))) (local (AST.Name "res") : argrefs)
    ret_


mkAST :: (?expHash :: Exp Float :->: Hash) =>
         MExp Float
      -> [Variable]
      -> AST.Module
mkAST e vs = mkASTWithExt (return ()) [("fun1",(e,vs))]

unsafeWiths :: VS.Storable a => [VS.Vector a] -> ([Ptr a] -> IO b) -> IO b
unsafeWiths vs = go vs id
  where go []     ps f = f (ps [])
        go (x:xs) ps f = VS.unsafeWith x $ \p -> go xs (ps . (p:)) f


callFn :: String -> [VS.Vector Float] -> ForeignPtr Float -> LLVMRunT IO ()
callFn name vargs fpr = do
  fn <- lookupFun name
  liftIO  . unsafeWiths vargs $ \ps -> do
    let vps = VS.fromList ps
    VS.MVector _ fparg <- VS.thaw vps
    withForeignPtr fparg $ \pargs ->
      withForeignPtr fpr $ \pres ->
        run fn pres pargs

runJITASTPrinter :: String 
                 -> (VS.Vector Float -> IO ())
                 -> AST.Module
                 -> [VS.Vector Float]
                 -> VS.Vector Float
                 -> LLVMContextT IO (Either String ())
runJITASTPrinter name printer ast vargs vres =
  runJIT name ast $ \mfn -> 
    case mfn of
      Nothing -> putStrLn "Nothing?"
      Just fn -> do
        unsafeWiths vargs $ \ps -> do
          let vps = VS.fromList ps
          VS.MVector _ fparg <- VS.thaw vps
          mv@(VS.MVector _ fpr) <- VS.thaw vres
          withForeignPtr fparg $ \pargs ->
            withForeignPtr fpr $ \pres -> do
              run fn pres pargs
              vr' <- VS.freeze mv
              printer vr'
