
module Language.Builtins where

import Eval.Values
import Language.AST
import TypeSystem.Types

import Data.Map (fromList, empty)

import System.IO (hFlush, stdout)
import Control.Monad.Trans (liftIO)


-- * Tabelas pré-definidas


builtinSymT  :: SymbolTable
builtinSymT  = empty

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
      ("pow"    , pow)
    , ("readInt", readInt)
   ]


-- * Utils

haskellProc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> ([Value] -> IO ())     -- ^ procedimento na monad IO
            -> Procedure
haskellProc c f   = HaskellProc c (liftIO . f)

haskellFunc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> Type                   -- ^ tipo de retorno da funcao
            -> ([Value] -> IO Value)  -- ^ funcao na monad IO
            -> Function
haskellFunc c t f = HaskellFunc c t (liftIO . f)



-- * Procedimentos pre-definidos


write :: Procedure
write = haskellProc check fun
 where
  check _  = True
  fun args = do let strs = map show args
                    str  = concat strs
                putStr str
                hFlush stdout


writeln :: Procedure
writeln = haskellProc check fun
 where
  check _  = True
  fun args = do let strs = map show args
                    str  = concat strs
                putStrLn str


-- * Funções pré-definidas


pow :: Function
pow = haskellFunc check IntegerT fun
 where
  check [IntegerT, IntegerT] = True
  check _                    = False
  
  fun [IntVal a, IntVal b]   = return (IntVal (a ^ b))


readInt :: Function
readInt = haskellFunc check IntegerT fun
 where
  check [] = True
  check _  = False
  
  fun [] = do num <- readLn
              return (IntVal num)
