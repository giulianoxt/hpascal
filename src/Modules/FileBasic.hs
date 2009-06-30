{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.FileBasic where

import Modules.Basic

import System.IO
import Control.Exception

filebasic :: PascalModule 
filebasic = emptyModule {
 mProcT = fromList [
    ("writeFile", writeFile'),
    ("appendFile", appendFile')	
   ],
 mFuncT = fromList [
	("readFile",  readFile')
   ]
}

writeFile' :: Procedure
writeFile' = haskellProc check fun
  where
    check [StringT, StringT] = True
    check _ = False

    fun [StringVal a, StringVal b] = writeFile a b

appendFile' :: Procedure
appendFile' = haskellProc check fun
  where
    check [StringT, StringT] = True
    check _ = False

    fun [StringVal a, StringVal b] = appendFile a b

	
readFile' :: Function
readFile' = haskellFunc check StringT fun
  where
    check [StringT] = True
    check _ = False

    fun [StringVal a] = do s <- (readFile a);
						   return (StringVal s)


