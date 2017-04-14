{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

--
import           Test.Delta
import           Test.Fib
import           Test.Rule
import           Test.Simple

separator s = do
  putStrLn "--------------------"
  putStrLn ("--    " ++ s )
  putStrLn "--------------------"

  
main :: IO ()
main = do
  -- delta_nosimplify
  -- putStr "\n\n\n\n\n"
  -- delta_simplify
  -- delta_eval
  -- test_differentiation
  separator "rule1"
  rule1
  separator "rule2"
  rule2
  separator "rule3"
  rule3
  separator "rule4"
  rule4


  -- separator "norule"
  -- norule
