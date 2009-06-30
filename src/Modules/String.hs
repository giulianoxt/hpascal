{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.String where

import Modules.Basic


string :: PascalModule 
string = emptyModule {
 mFuncT = fromList [
    ("strcat"  , strcat),
    ("strlen"  , strlen),
    ("strcmp"  , strcmp),
    ("substr"  , substr),
    ("strchr"  , strchr)
   ]
}


strcat :: Function
strcat = pureHaskellFunc check StringT fun
  where
    check [StringT, StringT] = True
    check _ = False

    fun [StringVal a, StringVal b] = StringVal (a++b)

strlen :: Function
strlen = pureHaskellFunc check IntegerT fun
  where
    check [StringT] = True
    check _ = False

    fun [StringVal a] = IntVal(length a)

strcmp :: Function
strcmp = pureHaskellFunc check IntegerT fun
  where
    check [StringT,StringT] = True
    check _ = False

    fun [StringVal a, StringVal b] = IntVal (if a == b then 0 else (if a < b then -1 else 1))


substr_haskell :: String -> Int -> Int -> Int -> String
substr_haskell (h:t) pos i e
  | (pos > e) = ""
  | (pos >= i) && (pos <= e) = h:(substr_haskell t (pos+1) i e)
  | (pos < i) = (substr_haskell t (pos+1) i e)

substr :: Function
substr = pureHaskellFunc check StringT fun
  where
    check [StringT,IntegerT,IntegerT] = True
    check _ = False
		
    fun [StringVal a,IntVal i, IntVal e] = StringVal (substr_haskell a 0 (i-1) (e-1))

strchr_haskell :: String -> Int -> Char -> Int -> Int

strchr_haskell [] _ _ _ = -1
strchr_haskell (h:t) ac ch pos
  | pos < 1 = -1
  | pos == 1 = if h == ch then ac else (strchr_haskell t (ac+1) ch 1)
  | pos > 1 = strchr_haskell t (ac+1) ch (pos-1)

strchr :: Function
strchr = pureHaskellFunc check IntegerT fun
  where
    check [StringT,CharT,IntegerT] = True
    check _ = False

    fun [StringVal a, CharVal ch, IntVal start] = IntVal (strchr_haskell a 1 ch start)
