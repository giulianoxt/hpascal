-- Modelagem das tabelas de símbolos
-- utilizadas no HPascal

module SymbolTable where

import Data.Map


type TypeTable = Map String Type

type SymbolTable = Map String (Value, String)

data Type = 
   IntegerT
 | BooleanT
 deriving (Show)

data Value = 
   VInt Int
 | VReal Double
 | VBool Bool
 deriving (Show)

builtinTypes   :: TypeTable
builtinTypes =
  insert "integer" IntegerT $
  insert "boolean" BooleanT $
  empty

builtinSymbols :: SymbolTable
builtinSymbols = empty

defaultV :: Type -> Value
defaultV IntegerT = VInt 0
defaultV BooleanT = VBool False
