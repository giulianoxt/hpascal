{-# LANGUAGE RankNTypes #-}

-- |Modulo contendo a definicao da arvore abstrata
-- de sintaxe do HPascal. Pedacos da arvore sao definidos
-- separadamente, usando construtores /data/. 
--
-- Esta arvore nao tem como objetivo ser construida
-- /na mao/, e sim pelo parser em "Parsing".
--
-- Note que as definicoes contidas aqui sao semelhantes a
-- gramatica presente na documentacao final do /HPascal/.

module Language.AST where

import Language.Basic
import Language.Scope (Scope)

import Eval.Values (Value)
import TypeSystem.Types (Type)

import Data.Map (Map)
import Control.Monad.Trans (MonadIO)


-- * Programas

-- | Raiz da arvore, representa um programa inteiro.
data Program = Program Identifier UsesClause Block
 deriving (Show)

-- | Importam nomes de funcoes e variaveis
-- globais externas para o escopo atual.
data UsesClause = UsesClause [Identifier]
 deriving (Show)

-- | Bloco de codigo. Esta presente no escopo principal de um programa
-- assim como em definicoes de /subrotinas/.
--
-- Contem uma secao de declaracoes e um 'CompoundStatement'.
data Block = Block DeclarationPart Statement [StaticData]
 deriving (Show)

-- * Declaracoes

-- | Secao de declaracoes de um 'Block'.
data DeclarationPart =
  DeclPart [ConstantDeclaration]
           [TypeDeclaration]
           [VariableDeclaration]
           [RoutineDeclaration]
 deriving (Show)

-- | Declaracao de constantes.
data ConstantDeclaration = ConstDec Identifier (Maybe Type) Expr
 deriving (Show)

-- | Declaracao de tipos.
data TypeDeclaration = TypeDec Identifier Type
 deriving(Show)

-- | Declaracao de variaveis de um mesmo tipo.
data VariableDeclaration = VarDec [Identifier] Type (Maybe Expr)
 deriving (Show)

-- | Declaracao de rotinas (procedimentos ou funcoes).
data RoutineDeclaration =
   ProcedureDec Identifier ProcSignature Block
 | FunctionDec  Identifier ProcSignature Identifier Block -- (inclui tipo de retorno)
 deriving (Show)

type ProcSignature = [Parameter]

-- | Declaracao dos parametros para as rotinas.
data Parameter = Parameter PassingMode [Identifier] Type (Maybe Expr)
 deriving (Show, Eq)
 
data PassingMode = Value | Const | Reference
 deriving (Show, Eq)

-- | Declaracao de arrays.
data ArrayDeclaration = ArrayDec [(Number, Number)]
 deriving (Show)

-- * Comandos

-- | Representacao interna dos comandos aceitos pela linguagem. Um
-- 'Statement' representa um bloco minimo de execucao de codigo.
data Statement =
   -- simple statements
   Nop
 | Assignment VariableReference Expr
 | ProcedureCall Identifier [Expr] Int
 
 -- control flow statements
 | Break | Continue | Raise (Maybe Expr)
 
 -- structured statements
 | Compound [Statement]
 | If Expr Statement Statement                  -- if (e) then stmt1 else stmt2
 | For ForUpdate Identifier Expr Expr Statement -- for i = a (to | downto) b do stmt
 | Repeat Statement Expr                        -- statement eh sempre um compound
 | While Expr Statement                         -- while (e) stmt
 | With [VariableReference] Statement           -- with v1,v2,v3 do stmt
 | Try Statement [ExceptionHandler] Statement
 | Case Expr [CasePart] 
 deriving (Show)

data ForUpdate = To | Downto
 deriving (Show)

data ExceptionHandler = Handler Expr Statement
 deriving (Show)
 
data CasePart =
   CaseClause [CaseMatch] Statement
 | ElseClause Statement
 deriving (Show)

data CaseMatch =
   SingleCase Constant
 | RangeCase Constant Constant
 deriving (Show)

-- * Expressoes

-- | Expressoes aceitas pela linguagem.
-- Faz uso de construtores prefixos e infixos para melhorar
-- a legibilidade dos casamentos de padrao.
--
-- Os operadores (construtores binarios) sao apresentados aqui
-- na ordem de precedencia da linguagem (menor a maior)
data Expr = 
   Expr :<: Expr         -- ^ Menor que
 | Expr :>: Expr         -- ^ Maior que
 | Expr :=: Expr         -- ^ Igual a
 | Expr :<=: Expr        -- ^ Menor ou igual a
 | Expr :>=: Expr        -- ^ Maior ou igual a
 | Expr :<>: Expr        -- ^ Diferente de
 | Expr `In` Expr        -- ^ Contido em
 
 | Expr :+: Expr         -- ^ Soma
 | Expr :-: Expr         -- ^ Subtracao
 | Expr `Or` Expr        -- ^ Ou logico / booleano
 | Expr `Xor` Expr       -- ^ Xor logico / booleano
 
 | Expr :*: Expr         -- ^ Multiplicacao
 | Expr :/: Expr         -- ^ Divisao (sempre retorna /real/)
 | Expr `Div` Expr       -- ^ Divisao inteira
 | Expr `Mod` Expr       -- ^ Modulo entre inteiros
 | Expr `And` Expr       -- ^ And logico / booleano
 | Expr `Shl` Expr       -- ^ Deslocamento binario a esquerda
 | Expr `Shr` Expr       -- ^ Deslocamento binario a direita
 
 | Expr :**: Expr        -- ^ Exponenciacao
 
 | Not Expr              -- ^ Negacao booleana
 | Minus Expr            -- ^ Menos unario
 
 | ConstExpr Constant    -- ^ Constante
 | Var VariableReference -- ^ Referencia a variavel
 deriving (Show, Eq)
  
-- | Por enquanto so como um desses tres tipos.
data Constant =
   ConstNum Number
 | ConstStr String
 | ConstBool Bool
 | ConstChar Char
 deriving (Show, Eq)

-- * Fatores simples

type Number = Int

-- | Operador de atribuicao ('Assignment').
-- Pode ser: "+=", "-=", ":=", "/=", "*="
type AssignOp = String

-- | Referencia para variavel.
-- Por enquanto so como um 'Identifier' normal.
type VariableReference = Identifier


-- * Tabelas

-- | Tabela interna de tipos. Mapeia identificadores para
-- tipos concretos.
--
-- Note que nem todos os tipos utilizados em um programa
-- estarao presentes nessa tabela, pois o HPascal permite
-- declaracao de variaveis com tipos anonimos (sem identificadores).
-- Ex: var x : array (3..5) of string;  
type TypeTable   = Map Identifier Type


-- | Tabela geral de simbolos (variaveis, subrotinas, etc.). Mapeia
-- identificadores para os tipos concretos dos simbolos.
--
-- Devido a declaracao com tipos anonimos, nem todos os tipos presentes
-- aqui estarao na tabela 'TypeTable'.
--
-- Nao guardamos o valor atual da variavel, por exemplo, pois as tabelas
-- deste modulo so serao utilizadas durante o parsing.
type SymbolTable = Map Identifier Type


type ProcedureTable = Map String Procedure


data StaticData = StaticData {
    stSymT  :: SymbolTable
  , stTypeT :: TypeTable
  , stProcT :: ProcedureTable
  , scope   :: Scope
 } deriving (Show)


data Procedure =
   Procedure   {
     overloads :: [ProcedureInstance]
   } 
 | HaskellProc {
     check     :: [Type]  -> Bool
   , fun       :: (MonadIO m) => [Value] -> m ()
   } 

instance (Show Procedure) where
  show (Procedure ps) = "Procedure " ++ show ps
  show (HaskellProc _ _) = "HaskellProcedure"


data ProcedureInstance = ProcInstance {
   signature :: ProcSignature
 , code      :: Block
} deriving (Show)

instance (Eq ProcedureInstance) where
  ProcInstance sig1 _ == ProcInstance sig2 _ = (sig1 == sig2)


matchProcCall :: [Type]
              -> [ProcedureInstance]
              -> [(ProcedureInstance, Int)]
matchProcCall types sigs = match (zip sigs [0..])
  where match [] = []
        match (sigP@((ProcInstance params _),_):ps)
          | match' types params = sigP : match ps
          | otherwise           = match ps
        
        match' [] []                                    =
          True              -- chamadas vazias
        match' []     ((Parameter _ [] _ _):ps)         =
          match' [] ps      -- n devia vir aqui eu acho
        match' []     ((Parameter _ [_] _ (Just _)):ps) =
          match' [] ps      -- usando parametro default
        match' (t:ts) ((Parameter _ [] _ _):ps)         =
          match' (t:ts) ps  -- passando pra outra secao
        match' (t:ts) ((Parameter m (_:idl) t' ex):ps)
          | t == t'   =     -- usando argumento
             match' ts ((Parameter m idl t' ex):ps)
          | otherwise =     -- nao casou
             False
        match' _ _ = False