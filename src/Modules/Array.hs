{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.Array where

import Modules.Basic

import Data.Array


array :: PascalModule
array = emptyModule {
  mFuncT = fromList [
       ("low", low)
     , ("high", high)
     , ("size", size)
    ]
}


low :: Function
low = pureHaskellFunc check IntegerT fun
 where
  check [ArrayT _ _] = True
  check _            = False
  
  fun [ArrayVal a]   = IntVal . fst $ bounds a


high :: Function
high = pureHaskellFunc check IntegerT fun
 where
  check [ArrayT _ _] = True
  check _            = False
  
  fun [ArrayVal a]   = IntVal . snd $ bounds a


size :: Function
size = pureHaskellFunc check IntegerT fun
 where
  check [ArrayT _ _] = True
  check _            = False
  
  fun [ArrayVal v]   = IntVal (b - a) where (a, b) = bounds v
