{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Modules.Math where

import Modules.Basic


math :: PascalModule 
math = emptyModule {
 mFuncT = fromList [
      ("pow"  , pow)
    , ("round", round')
    , ("sin"  , sin')
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
    , ("succ", succ')
    , ("pred", pred')
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