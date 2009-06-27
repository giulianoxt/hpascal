{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Language.Builtins where

import Eval.Values
import Language.AST
import TypeSystem.Types

import Data.Char
import Data.Map (fromList, empty)

import System.IO
import Control.Exception
import Control.Monad.Trans (liftIO)


-- * Tabelas pré-definidas


builtinSymT  :: SymbolTable
builtinSymT  = empty

builtinTypeT :: TypeTable
builtinTypeT = fromList [
     ("integer", IntegerT)
   , ("boolean", BooleanT)
   , ("string" , StringT)
   , ("real"   , FloatT)
   , ("char"   , CharT)
  ]

builtinProcT :: ProcedureTable
builtinProcT = fromList [
      ("write"  , write)
    , ("writeln", writeln)
   ]

builtinFuncT :: FunctionTable
builtinFuncT = fromList [
      ("readint", readint)
    , ("readfloat", readfloat)
    , ("pow"    , pow)
    , ("round"  , round')
    , ("sin", sin')
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
	]


-- * Utils


haskellProc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> ([Value] -> IO ())     -- ^ procedimento na monad IO
            -> Procedure
haskellProc c f   = HaskellProc c (liftIO . f)


haskellFunc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> Type                   -- ^ tipo de retorno da funcao
            -> ([Value] -> IO Value)  -- ^ funcao na monad IO
            -> Function
haskellFunc c t f = HaskellFunc c t (liftIO . f)


pureHaskellFunc :: ([Type] -> Bool)   -- ^ checagem da assinatura
                -> Type               -- ^ tipo de retorno
                -> ([Value] -> Value) -- ^ funcao pura
                -> Function
pureHaskellFunc c t f = HaskellFunc c t (return . f)


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


readint :: Function
readint = haskellFunc check IntegerT fun
 where
  check [] = True
  check _  = False
  
  fun []   = do skip isSpace
                num <- skip isDigit
                return (IntVal (read num))
  
  skip f   = do look <- try $ hLookAhead stdin
                case look of
                 Left (_ :: IOError) -> return []
                 Right c
                  | f c              ->
                         do hGetChar stdin
                            cs <- skip f
                            return (c : cs)
                  | otherwise        ->
                         return []


readfloat :: Function
readfloat = haskellFunc check FloatT fun
 where
  check [] = True
  check _  = False
  
  fun []   = do num <- readLn
                return (FloatVal num)


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
		
sin' :: Function
sin' = pureHaskellFunc check FloatT fun
	where
		check [FloatT] = True
		check _ = False
		
		fun [FloatVal n] = FloatVal (Prelude.sin n)
		
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
		