
module Language.Builtins where

import Eval.Values

import Language.AST
import TypeSystem.Types

import Data.Map (fromList, empty)
import Control.Monad.Trans (liftIO)



builtinSymT :: SymbolTable
builtinSymT = empty

builtinTypeT :: TypeTable
builtinTypeT = fromList [
     ("integer", IntegerT)
   , ("boolean", BooleanT)
   , ("string" , StringT)
  ]

builtinProcT :: ProcedureTable
builtinProcT = fromList [
      ("write"  , write)
    , ("writeln", writeln)
   ]

builtinFuncT :: FunctionTable
builtinFuncT = fromList [
      ("pow" , pow)
   ]


-- * Procedimentos pre-definidos

write :: Procedure
write = HaskellProc check fun
 where
  check _  = True
  fun args = let strs = map show args
                 str  = concat strs in
             liftIO (putStr str)


writeln :: Procedure
writeln = HaskellProc check fun
 where
  check _  = True
  fun args = let strs = map show args
                 str  = concat strs in
             liftIO (putStrLn str)


-- * Funções pré-definidas

pow :: Function
pow = HaskellFunc check IntegerT fun
 where
  check [IntegerT, IntegerT] = True
  check _                    = False
  
  fun [IntVal a, IntVal b]   = return (IntVal (a ^ b))
