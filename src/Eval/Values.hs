module Eval.Values where

import TypeSystem.Types

import Data.Map


data Value =
   IntVal  Int
 | BoolVal Bool
 | StringVal String
 
type ValueTable = Map Identifier Value


 
instance Show Value where
  show (IntVal  n) = show n
  show (BoolVal b) = show b
  show (StringVal s) = s


defVal :: Type -> Value
defVal IntegerT = IntVal 0
defVal BooleanT = BoolVal False
defVal _        = error "Runtime.defVal"


type UnaryOp  = Value -> Value

type BinaryOp = Value -> Value -> Value


numOp1  :: (Int -> Int) -> UnaryOp
numOp1 f (IntVal n) = IntVal (f n)
numOp1 _ _          = error "Values.numOp1"


boolOp1 :: (Bool -> Bool) -> UnaryOp
boolOp1 f (BoolVal b) = BoolVal (f b)
boolOp1 _ _           = error "Values.boolOp1"


numOp2  :: (Int -> Int -> Int) -> BinaryOp
numOp2 f (IntVal a) (IntVal b) = IntVal (f a b)
numOp2 _ _ _                   = error "Values.numOp2"


boolOp2 :: (Bool -> Bool -> Bool) -> BinaryOp
boolOp2 f (BoolVal a) (BoolVal b) = BoolVal (f a b)
boolOp2 _ _ _                     = error "Values.boolOp2"


relOp2  :: (Int -> Int -> Bool) -> BinaryOp
relOp2 f (IntVal a) (IntVal b) = BoolVal (f a b)
relOp2 _ _ _                   = error "Values.relOp2"


eqOp2   :: BinaryOp
eqOp2 (IntVal a)  (IntVal b)  = BoolVal (a == b)
eqOp2 (BoolVal a) (BoolVal b) = BoolVal (a == b)
eqOp2 _ _                     = error "Values.eqOp2"
