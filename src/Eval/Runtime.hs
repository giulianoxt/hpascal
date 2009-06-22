
module Eval.Runtime where

import Eval.Values
import Language.Tables
import TypeSystem.Types (Identifier)

import Data.Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup)



type ValueTable = Map Identifier Value

 
data RuntimeEnv = RuntimeEnv {
   symT  :: SymbolTable
 , typeT :: TypeTable
 , valT  :: ValueTable
} deriving (Show)


updateRuntimeEnv :: StaticData -> RuntimeEnv -> RuntimeEnv
updateRuntimeEnv (newSymT, newTypeT) runtimeEnv =
  runtimeEnv {
     symT  = union newSymT oldSymT
   , typeT = union newTypeT oldTypeT
  }

  where
    oldSymT  = symT  runtimeEnv
    oldTypeT = typeT runtimeEnv
    

updateValTable :: Identifier -> Value -> RuntimeEnv -> RuntimeEnv
updateValTable ident val re =
  re { valT = insert ident val oldValT }
 where
  oldValT = valT re


insertVarDefVal :: Identifier -> RuntimeEnv -> RuntimeEnv
insertVarDefVal ident re =
  re { valT = newValT }
  
  where symTab    = symT re
        Just varT = lookup ident symTab
        varVal    = defVal varT
        oldValT   = valT re
        newValT   = insert ident varVal oldValT  


varValue :: Identifier -> RuntimeEnv -> Value
varValue ident re = fromJust (lookup ident valTab)
 where valTab = valT re

  


defaultRuntimeEnv :: RuntimeEnv
defaultRuntimeEnv =
  RuntimeEnv {
     symT  = empty
   , typeT = empty
   , valT  = empty
  }
