
module Eval.Runtime where

import Language.Tables
import TypeSystem.Types

import Data.Map (Map, empty, union)


data Value =
   IntVal  Int
 | BoolVal Bool
 deriving (Show, Eq)
 
 
type ValueTable = Map Identifier Value

 
data RuntimeEnv = RuntimeEnv {
   symT  :: SymbolTable
 , typeT :: TypeTable
 , valT  :: ValueTable
} deriving (Show)


updateRuntimeEnv :: StaticData -> RuntimeEnv -> RuntimeEnv
updateRuntimeEnv (symT, typeT) runtimeEnv =
  runtimeEnv {
     symT  = union symT oldSymT
   , typeT = union typeT oldTypeT
  }

  where
    oldSymT  = symT  runtimeEnv
    oldTypeT = typeT runtimeEnv
    

updateValTable :: Identifier -> Value -> RuntimeEnv -> RuntimeEnv
updateValTable ident val re =
  re { valT = insert ident val oldValT }
 where
  oldValT = valT re






defaultRuntimeEnv :: RuntimeEnv
defaultRuntimeEnv =
  RuntimeEnv {
     symT  = empty
   , typeT = empty
   , valT  = empty
  }


defVal :: Type -> Value
defVal IntegerT = IntVal 0
defVal BooleanT = BoolVal False
defVal _        = error "Runtime.defVal"


