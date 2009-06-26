-- | Contem a implementacao concreta dos tipos
-- presentes na linguagem HPascal.
--
-- Define as estruturas de dados utilizadas para
-- representacao dos tipos HPascal, assim como funcoes
-- de coercao adequadas.

module TypeSystem.Types where

import Prelude hiding (fail)


-- * Definicoes de tipos


-- | Tipo variante em Haskell usado para representar os tipos
-- concretos presentes em programas HPascal
--
-- O tipo indefinido ('UnknownType') e utilizado quando a
-- declaracao da variavel contem um tipo ilegal
-- Exemplo: /var x : TipoNaoDefinido;/
--
-- Assim, podemos continuar o processo de parsing e analise,
-- possivelmente encontrando mais erros, em vez de parar na
-- primeira ocorrencia.
data Type = 
   IntegerT     -- ^ Inteiro
 | BooleanT     -- ^ Booleano
 | FloatT
 | StringT
 | UnknownType  -- ^ Tipo indefinido
 deriving (Eq)
 
instance (Show Type) where
  show FloatT      = "real"
  show IntegerT    = "integer"
  show BooleanT    = "boolean"
  show StringT     = "string"
  show UnknownType = "UnknownType"



-- * Coercoes


-- ** Tipos de coercoes


-- | Resultado de uma operacao de coercao.
--  - Left indica erro, contendo a mensagem correspondente
--  - Right indica sucesso, contendo o tipo resultante da coercao
type CoerceResult   = Either String Type


-- | Tipo das funcoes de coercao para operadores unarios
type UnaryCoercion  = Type -> CoerceResult


-- | Tipo das funcoes de coercao para operadores binarios
type BinaryCoercion = Type -> Type -> CoerceResult


-- ** Funcoes auxiliares as coercoes


-- | Indica sucesso em uma coercao, com o resultado 'Type' indicado
suceed :: Type -> CoerceResult
suceed = Right

-- | Indica falha em uma coercao, com a mensagem dada e 
-- o tipo esperado dado
fail :: (Show a) => String -> a -> CoerceResult
fail msg t = Left $ "Expected " ++ msg ++ ", got " ++ show t

-- | Indica se um tipo e numerico
isNumeric    :: Type -> Bool
isNumeric IntegerT = True
isNumeric FloatT   = True
isNumeric _        = False


-- ** Funcoes de coercao


-- | Operador numerico unario (ex: menos unario)
cNumOp1  :: UnaryCoercion
cNumOp1 t
 | isNumeric t = suceed t
 | otherwise   = fail "a numeric type" t


-- | Operador booleano unario (ex: not)
cBoolOp1 :: UnaryCoercion
cBoolOp1 BooleanT = suceed BooleanT
cBoolOp1 t        = fail "a boolean type" t


-- | Operador numerico binario normal (ex: *, +, -)
cNumOp2  :: BinaryCoercion
cNumOp2 IntegerT FloatT   = suceed FloatT
cNumOp2 FloatT   IntegerT = suceed FloatT
cNumOp2 t1 t2
 | t1 == t2 && isNumeric t1 = suceed t1
 | otherwise                =
    fail "two numeric types on numeric operation" (t1,t2)

cDivOp2 :: BinaryCoercion
cDivOp2 t1 t2
  | all isNumeric [t1,t2] = suceed FloatT
  | otherwise             =
    fail "two numeric types on numeric operation" (t1, t2)


-- | Operador booleano binario normal (ex: and, or)
cBoolOp2 :: BinaryCoercion
cBoolOp2 BooleanT BooleanT = suceed BooleanT
cBoolOp2 t1 t2             =
    fail "two boolean types on boolean operation" (t1, t2)


-- | Operador relacional binario normal (ex: <)
cRelOp2  :: BinaryCoercion
cRelOp2 t1 t2
 | all isNumeric [t1,t2] = suceed BooleanT
 | otherwise             =
    fail "two numeric types on relational operation" (t1, t2)


-- | Operador binario de igualdade
cEqOp2   :: BinaryCoercion
cEqOp2 t1 t2
  | t1 == t2  = suceed BooleanT
  | otherwise = fail "two equal types" (t1, t2)


-- | Atribuicao
cAssign :: BinaryCoercion
cAssign t1 t2
  | t1 == t2  = suceed t1
  | otherwise = fail "two compatible types" (t1, t2)
        