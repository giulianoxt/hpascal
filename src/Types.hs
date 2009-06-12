module Types where

import Prelude hiding (fail)

-- | Tipo variante em Haskell usado para representar os tipos
-- concretos presentes em programas HPascal
--
-- O tipo indefinido ('UnknownType') e utilizado quando a declaracao
-- da variavel contem um tipo ilegal (ex: var x : TipoNaoDefinido;).
--
-- Assim, podemos continuar o processo de parsing e analise, possivelmente
-- encontrando mais erros, em vez de parar na primeira ocorrencia.
data Type = 
   IntegerT     -- ^ Inteiro
 | BooleanT     -- ^ Booleano
 | UnknownType  -- ^ Tipo indefinido
 deriving (Show, Eq)


type CoerceResult   = Either String Type


type UnaryCoercion  = Type -> CoerceResult


type BinaryCoercion = Type -> Type -> CoerceResult


suceed :: Type -> CoerceResult
suceed = Right

fail :: (Show a) => String -> a -> CoerceResult
fail msg t = Left $ "Expected " ++ msg ++ ", got " ++ show t


isNumeric    :: Type -> Bool
isNumeric IntegerT = True
isNumeric _        = False


cNumOp1  :: UnaryCoercion
cNumOp1 IntegerT = suceed IntegerT
cNumOp1 t        = fail "numeric type" t


cBoolOp1 :: UnaryCoercion
cBoolOp1 BooleanT = suceed BooleanT
cBoolOp1 t        = fail "boolean type" t


cNumOp2  :: BinaryCoercion
cNumOp2 t1 t2 = case (t1, t2) of
  (IntegerT, IntegerT) -> suceed IntegerT
  _                    -> fail "two numeric types" (t1,t2)


cBoolOp2 :: BinaryCoercion
cBoolOp2 BooleanT BooleanT = suceed BooleanT
cBoolOp2 t1 t2             = fail "two boolean types" (t1, t2)


cRelOp2  :: BinaryCoercion
cRelOp2 IntegerT IntegerT = suceed BooleanT
cRelOp2 t1 t2             = fail "two boolean types" (t1, t2)

cEqOp2   :: BinaryCoercion
cEqOp2 t1 t2
  | t1 == t2  = suceed BooleanT
  | otherwise = fail "two equal types" (t1, t2)
