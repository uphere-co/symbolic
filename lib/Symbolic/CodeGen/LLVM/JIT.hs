{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Symbolic.CodeGen.LLVM.JIT where

import           Control.Monad                      ( foldM )
import           Control.Monad.IO.Class             ( MonadIO(liftIO) )
import           Control.Monad.Reader.Class         ( MonadReader(ask) )
import           Control.Monad.Trans.Either         ( EitherT(..) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader         ( ReaderT(..) )
import           Data.HashMap.Strict                ( HashMap )
import qualified Data.HashMap.Strict         as HM
import           Foreign.Ptr                        (FunPtr, Ptr, castFunPtr)
import qualified LLVM.General.AST            as AST
import           LLVM.General.Context
import qualified LLVM.General.ExecutionEngine as EE
import           LLVM.General.Module         as Mod
import           LLVM.General.PassManager

type JITFunction = Ptr Float -> Ptr (Ptr Float) -> IO ()


foreign import ccall "dynamic" haskFun :: FunPtr JITFunction -> JITFunction 

type LLVMContextT m = ReaderT Context m

runLLVMContextT = runReaderT

run :: FunPtr a -> JITFunction
run fn = haskFun (castFunPtr fn :: FunPtr JITFunction)

jit :: (Context -> EE.MCJIT -> IO a) -> LLVMContextT IO a
jit action = do c <- ask 
                liftIO $ EE.withMCJIT c optlevel model ptrelim fastins (action c)
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: String
       -> AST.Module
       -> (Maybe (FunPtr ()) -> IO b)
       -> LLVMContextT IO (Either String b)
runJIT name mod' action = do
  jit $ \context executionEngine -> do
      r <- runExceptT $ withModuleFromAST context mod' $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          _optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          -- putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mfn <- EE.getFunction ee (AST.Name name)
            action mfn
      case r of
        Left err -> putStrLn err >> return r
        Right _ -> return r

type LLVMRunT m = EitherT String (ReaderT (HashMap String (FunPtr ())) m)

lookupFun :: (Monad m) => String -> LLVMRunT m (FunPtr ())
lookupFun n = do 
  fnmap <- ask
  EitherT . return . maybe (Left (n++" is not registered.")) Right . HM.lookup n $ fnmap
  

compileNRun :: [String] -> AST.Module -> LLVMRunT IO b -> LLVMContextT IO (Either String b)
compileNRun names mod' action = do
  jit $ \context executionEngine -> do
      runExceptT $ withModuleFromAST context mod' $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          _optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          -- putStrLn s
          
          EE.withModuleInEngine executionEngine m $ \ee -> do
            let errf n = maybe (Left (n ++ " is missing")) Right
                f fm n = do
                  fn <- EitherT $ errf n <$> EE.getFunction ee (AST.Name n)
                  return (HM.insert n fn fm)
            e1 <- runEitherT (foldM f HM.empty names)
            case e1 of
              Left err -> error err
              Right fnmap -> do
                e2 <- runReaderT (runEitherT action) fnmap
                case e2 of
                  Left err -> error err
                  Right r -> return r

