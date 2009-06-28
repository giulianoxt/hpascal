-- | Modulo contendo algumas definicoes basicas
-- a serem utilizadas na construcao da arvore
-- sintatica do modulo 'AST'.
module Language.Basic where

-- | Numero (inteiro ou de ponto flutuante).
data Number =
   IntNum   Int
 | FloatNum Float
 deriving (Show, Eq)

-- | Operador de atribuicao.
-- Pode ser: "+=", "-=", ":=", "/=", "*="
type AssignOp = String

-- | Referencia para uma variavel, que
-- consiste no identificador da variavel.
type VariableReference = Identifier

-- | Identificador de alguma entidade.
type Identifier = String
