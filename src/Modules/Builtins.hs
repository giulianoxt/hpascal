{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.Builtins where

import Modules.Basic

import Data.Char
import Data.Map hiding (map)

import System.IO
import Control.Exception


-- * Tabelas pré-definidas

builtinModule :: PascalModule
builtinModule = HaskellModule {
   mSymT  = empty
 , mTypeT = fromList [
      ("integer", IntegerT)
    , ("boolean", BooleanT)
    , ("string" , StringT)
    , ("real"   , FloatT)
    , ("char"   , CharT)
   ]
 , mProcT = fromList [
      ("write"  , write)
    , ("writeln", writeln)
   ]
 , mFuncT = fromList [
      ("readint", readint)
    , ("readfloat", readfloat)
    , ("ord", ord')
    , ("chr", chr')
   ]
}


-- * Procedimentos pre-definidos


write :: Procedure
write = haskellProc check fun
 where
  check _  = True
  fun args = do let strs = map show args
                    str  = concat strs
                putStr str
                hFlush stdout


writeln :: Procedure
writeln = haskellProc check fun
 where
  check _  = True
  fun args = do let strs = map show args
                    str  = concat strs
                putStrLn str


-- * Funções pré-definidas


lexem :: (Read a) => IO a
lexem = do skip isSpace
           x <- skip (not . isSpace)
           return (read x)
 where
  skip f = do look <- try $ hLookAhead stdin
              case look of
               Left (_ :: IOError) -> return []
               Right c
                | f c       -> do hGetChar stdin
                                  cs <- skip f
                                  return (c : cs)
                | otherwise -> return []


readint :: Function
readint = haskellFunc check IntegerT fun
 where
  check [] = True
  check _  = False

  fun []   = do num <- lexem
                return (IntVal num)


readfloat :: Function
readfloat = haskellFunc check FloatT fun
 where
  check [] = True
  check _  = False
  
  fun []   = do num <- lexem
                return (FloatVal num)


ord' :: Function
ord' = pureHaskellFunc check IntegerT fun
  where
    check [CharT] = True
    check _ = False

    fun [CharVal n] = IntVal (Data.Char.ord n)

chr' :: Function
chr' = pureHaskellFunc check CharT fun
  where
    check [IntegerT] = True
    check _ = False

    fun [IntVal n] = CharVal (Data.Char.chr n)
