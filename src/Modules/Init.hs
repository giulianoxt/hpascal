
module Modules.Init where

import Language.AST
import Language.Basic

import Modules.Math
import Modules.Array
import Modules.String
import Modules.Builtins

import Data.Map


builtin :: PascalModule
builtin = builtinModule

modules :: Map Identifier PascalModule
modules = fromList [
     ("Builtin", builtin)
   , ("Math"   , math)
   , ("String" , string)
   , ("Array"  , array)
 ]
