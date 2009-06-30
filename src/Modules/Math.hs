{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.Math where

import Language.AST
import Modules.Basic

import Eval.Values
import TypeSystem.Types

import Data.Map


math :: PascalModule 
math = HaskellModule {
   mSymT  = empty
 , mTypeT = empty
 , mProcT = empty
 , mFuncT = fromList [
      ("pow"  , pow)
    , ("round", round')
    , ("sin"  , sin')
   ]
}


pow :: Function
pow = pureHaskellFunc check IntegerT fun
 where
  check [IntegerT, IntegerT] = True
  check _                    = False
  
  fun [IntVal a, IntVal b]   = IntVal (a ^ b)


round' :: Function
round' = pureHaskellFunc check IntegerT fun
 where
  check [FloatT]   = True
  check _          = False
  
  fun [FloatVal n] = IntVal (round n)

                
sin' :: Function
sin' = pureHaskellFunc check FloatT fun
  where
    check [FloatT] = True
    check _ = False
                
    fun [FloatVal n] = FloatVal (Prelude.sin n)

