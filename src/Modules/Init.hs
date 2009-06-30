
module Modules.Init where

import Language.AST
import Language.Basic

import Modules.Builtins
import Modules.Math

import Data.Map


builtin :: PascalModule
builtin = builtinModule

modules :: Map Identifier PascalModule
modules = fromList [
     ("Builtin", builtin)
   , ("Math"   , math)
 ]
