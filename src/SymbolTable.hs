-- Modelagem das tabelas de símbolos
-- utilizadas no HPascal

module SymbolTable where

import Data.Map


type TypeTable = Map String Type

type SymbolTable = Map String (Value, String)

data Type = 
   IntegerT
 | BooleanT

data Value = 
   VInt Int
 | VReal Double
 | VBool Bool

builtinTypes   :: TypeTable
builtinTypes =
  insert "integer" IntegerT $
  insert "boolean" BooleanT $
  empty

builtinSymbols :: SymbolTable
builtinSymbols = empty
