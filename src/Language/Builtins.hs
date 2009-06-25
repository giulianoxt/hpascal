
module Language.Builtins where

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


-- * Procedimentos pre-definidos

write :: Procedure
write = HaskellProc {
   check = \_    -> True
 , fun   = \args ->  
             let strs = map show args
                 str  = concat strs   in
             liftIO (putStr str)
}

writeln :: Procedure
writeln = HaskellProc {
   check = \_    -> True
 , fun   = \args ->
             let strs = map show args
                 str  = concat strs   in
             liftIO (putStrLn str)
}
