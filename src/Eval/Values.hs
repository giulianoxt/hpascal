
module Eval.Values where

import Language.Basic
import TypeSystem.Types

import Data.Map
import Prelude hiding (map)


data Value =
   IntVal  Int
 | BoolVal Bool
 | CharVal Char
 | FloatVal Float
 | StringVal String
 | RecordVal (Map Identifier Value)
 
type ValueTable = Map Identifier Value


 
instance Show Value where
  show (IntVal  n)   = show n
  show (BoolVal b)   = show b
  show (CharVal c)   = [c]
  show (FloatVal f)  = show f
  show (StringVal s) = s
  show (RecordVal m) = "record " ++ show (toList m)


defVal :: Type -> Value
defVal IntegerT    = IntVal    0
defVal FloatT      = FloatVal  0.0
defVal BooleanT    = BoolVal   False
defVal CharT       = CharVal   '\0'
defVal StringT     = StringVal ""
defVal (RecordT m) = RecordVal (map defVal m)
defVal _           = error "Eval.Values.defVal"


type UnaryOp  = Value -> Value

type BinaryOp = Value -> Value -> Value


-- A intencao era:
-- 
-- numOp1  :: (Num a, Num b) => (a -> b) -> UnaryOp
-- numOp1 f (IntVal n)   = IntVal (f n)
-- numOp1 f (FloatVal n) = FloatVal (f n)
-- numOp1 _ _            = error "Eval.Values.numOp1"
-- 
-- nao entendi pq o typechecker n aceita

numOp1 :: (Int -> Int)
       -> (Float -> Float)
       -> UnaryOp
numOp1 f _ (IntVal n)   = IntVal (f n)
numOp1 _ g (FloatVal n) = FloatVal (g n)
numOp1 _ _ _ = error "Eval.Values.numOp1"


boolOp1 :: (Bool -> Bool) -> UnaryOp
boolOp1 f (BoolVal b) = BoolVal (f b)
boolOp1 _ _           = error "Eval.Values.boolOp1"


numOp2 :: (Int -> Int -> Int)
       -> (Float -> Float -> Float)
       -> BinaryOp
numOp2 f _ (IntVal a) (IntVal b)     = IntVal   (f a b)
numOp2 _ g (FloatVal a) (FloatVal b) = FloatVal (g a b)
numOp2 _ g (IntVal a) (FloatVal b)   = FloatVal (g (fromIntegral a) b)
numOp2 _ g (FloatVal a) (IntVal b)   = FloatVal (g a (fromIntegral b))
numOp2 _ _ _ _ = error "Eval.Values.numOp2N"


divOp2 :: BinaryOp
divOp2 (FloatVal a) (FloatVal b) = FloatVal (a / b)
divOp2 (IntVal a) (FloatVal b)   = FloatVal (fromIntegral a / b)
divOp2 (FloatVal a) (IntVal b)   = FloatVal (a / fromIntegral b)
divOp2 (IntVal a) (IntVal b)     = FloatVal (fromIntegral a /
                                             fromIntegral b)
divOp2 _ _ = error "Eval.Values.divOp2"


boolOp2 :: (Bool -> Bool -> Bool) -> BinaryOp
boolOp2 f (BoolVal a) (BoolVal b) = BoolVal (f a b)
boolOp2 _ _ _                     = error "Eval.Values.boolOp2"


relOp2 :: (Int -> Int -> Bool)
       -> (Float -> Float -> Bool)
       -> BinaryOp
relOp2 f _ (IntVal a) (IntVal b)     = BoolVal (f a b)
relOp2 _ g (FloatVal a) (FloatVal b) = BoolVal (g a b)
relOp2 _ g (IntVal a) (FloatVal b)   = BoolVal (g (fromIntegral a) b)
relOp2 _ g (FloatVal a) (IntVal b)   = BoolVal (g a (fromIntegral b))
relOp2 _ _ _ _                   = error "Eval.Values.relOp2"


eqOp2   :: BinaryOp
eqOp2 (IntVal  a) (IntVal  b)     = BoolVal (a == b)
eqOp2 (BoolVal a) (BoolVal b)     = BoolVal (a == b)
eqOp2 (FloatVal a) (FloatVal b)   = BoolVal (a == b)
eqOp2 (StringVal a) (StringVal b) = BoolVal (a == b)
eqOp2 _ _                     = error "Eval.Values.eqOp2"
