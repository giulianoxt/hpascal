{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.Builtins where

import Eval.Values
import Language.AST
import TypeSystem.Types
import Modules.Basic

import Data.Char
import Data.Array
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
    , ("cos", cos')     
    , ("tan", tan')             
    , ("arcsin", arcsin)
    , ("arccos", arccos)
    , ("arctan", arctan)        
    , ("odd", odd')     
    , ("even", even')   
    , ("absi", absi)
    , ("absf", absf)
    , ("sqr", sqr')
    , ("sqrt", sqrt')
    , ("log", log')
    , ("not", not')     
    , ("succ", succ')
    , ("pred", pred')
    , ("ord", ord')
    , ("chr", chr')     
    , ("low", low)
    , ("high", high)
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


absi :: Function
absi = pureHaskellFunc check IntegerT fun
	where
		check [IntegerT] = True
		check _ = False
		
		fun [IntVal n] = IntVal (abs n)

absf :: Function
absf = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (abs n)
		
cos' :: Function
cos' = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.cos n)
		
tan' :: Function
tan' = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.tan n)

		
arcsin :: Function
arcsin = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.asin n)
		
arccos :: Function
arccos = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.acos n)

arctan :: Function
arctan = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.atan n)

odd' :: Function
odd' = pureHaskellFunc check BooleanT fun
	where
		check [IntegerT] = True
		check _ = False
		
		fun [IntVal n] = BoolVal (Prelude.odd n)		
		
even' :: Function
even' = pureHaskellFunc check BooleanT fun
	where
		check [IntegerT] = True
		check _ = False
		
		fun [IntVal n] = BoolVal (Prelude.even n)		

sqr' :: Function
sqr' = pureHaskellFunc check IntegerT fun
	where
		check [IntegerT] = True
		check [FloatT] = True
		check _ = False
		
		fun [IntVal n] = IntVal (n * n)
		fun [FloatVal n] = FloatVal (n * n)
		
sqrt' :: Function
sqrt' = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.sqrt n)
		
log' :: Function
log' = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.log n)
		
not' :: Function
not' = pureHaskellFunc check BooleanT fun
	where
		check [BooleanT] = True
		check _ = False
		
		fun [BoolVal n] = BoolVal (Prelude.not n)

succ' :: Function
succ' = pureHaskellFunc check IntegerT fun
	where
		check [IntegerT] = True
		check _ = False
		
		fun [IntVal n] = IntVal (n+1)
		
pred' :: Function
pred' = pureHaskellFunc check IntegerT fun
	where
		check [IntegerT] = True
		check _ = False
		
		fun [IntVal n] = IntVal (n-1)
		
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

low :: Function
low = pureHaskellFunc check IntegerT fun
 where
  check [ArrayT _ _] = True
  check _            = False
  
  fun [ArrayVal a] = IntVal . fst $ bounds a

high :: Function
high = pureHaskellFunc check IntegerT fun
 where
  check [ArrayT _ _] = True
  check _            = False
  
  fun [ArrayVal a] = IntVal . snd $ bounds a
