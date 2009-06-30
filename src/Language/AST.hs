{-# LANGUAGE RankNTypes #-}

-- | Modulo contendo a definicao da arvore abstrata
-- de sintaxe do HPascal. Pedacos da arvore sao definidos
-- separadamente, usando construtores /data/. 
--
-- Esta arvore nao tem como objetivo ser construida
-- /na mao/, e sim pelo parser em "Parsing".
--
-- Note que as definicoes contidas aqui sao semelhantes a
-- gramatica EBNF presente na documentacao final do /HPascal/.

module Language.AST where

import Language.Basic
import Language.Scope (Scope)

import Eval.Values (Value)
import TypeSystem.Types (Type)

import Data.Map (Map)
import Control.Monad.Trans (MonadIO)


-- * Programas

-- | Raiz da arvore, representa um programa inteiro.
data Program = Program Identifier UsesClause Block StaticData
 deriving (Show)

-- | Clausulas de uso. Importam nomes de funcoes e
-- variaveis globais externas para o escopo atual.
data UsesClause = UsesClause [Identifier]
 deriving (Show)

-- | Bloco de codigo. Esta presente no escopo principal de
-- um programa assim como em definicoes de /subrotinas/.
--
-- Contem uma secao de declaracoes e uma sequencia de comandos
-- (um Statement pode ser um encadeamento de comandos, atraves
-- do seu construtor Compound).
--
-- Tambem sao guardados conjuntos de tabelas, do tipo 'StaticData',
-- que sao uteis as associacoes de identificadores a entidades,
-- a serem realizadas no ambiente de referenciamento do bloco.
data Block = Block DeclarationPart Statement [StaticData]

instance (Show Block) where
  show (Block decls stmt _) =
   "Block " ++ show decls ++ " " ++ show stmt

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
 deriving (Show)

-- | Declaracao de variaveis de um mesmo tipo.
data VariableDeclaration = VarDec [Identifier] Type (Maybe Expr)
 deriving (Show)

-- | Declaracao de rotinas (procedimentos ou funcoes).
data RoutineDeclaration =
   ProcedureDec Identifier RoutineSignature Block
 | FunctionDec  Identifier RoutineSignature Type Block -- (inclui tipo de retorno)
 deriving (Show)

-- | Assinatura de rotinas (lista dos seus parametros).
type RoutineSignature = [Parameter]

-- | Declaracao generica dos parametros para as rotinas.
data Parameter = Parameter PassingMode [Identifier] Type (Maybe Expr)
 deriving (Show, Eq)

-- | Modo de passagem dos parametros.
data PassingMode = Value | Const | Reference
 deriving (Show, Eq)


-- * Comandos

-- | Representacao interna dos comandos aceitos pela linguagem. Um
-- 'Statement' eh definido aqui como um ou mais comandos encadeados.
data Statement =
   -- Comandos simples
   Nop                                 -- Nenhuma operacao
 | Assignment VariableReference Expr   -- Atribuição
 | FunctionReturn Identifier Expr      -- Chamada de funcao
 | ProcedureCall Identifier [Expr] Int -- Chamada de procedimento
 
 -- Comandos de controle de fluxo
 | Break | Continue | Raise (Maybe Expr)
 
 -- Comandos estruturados
 | Compound [Statement]                             -- Sequencia de comandos
 | If Expr Statement Statement                      -- if (e) then stmt1 else stmt2
 | For ForUpdate VariableReference Expr Expr Statement     -- for i = a (to | downto) b do stmt
 | Repeat Statement Expr                            -- repeat (stmts) until (expr)
 | While Expr Statement                             -- while (e) stmt
 | With [VariableReference] Statement               -- with v1,v2,v3 do stmt
 | Try Statement (Maybe ExceptionHandler) Statement -- Comando /try except/
 | Case Expr [CasePart]                             -- Comando /case of/
 deriving (Show)

-- | Tipos de iteracao do laco 'for'.
data ForUpdate = To | Downto
 deriving (Show)

-- | Tratamento de excecoes.
data ExceptionHandler = Handler Expr Statement -- on (expr) do (stmt)
 deriving (Show)
 
-- | Definicao auxiliar para o comando 'case'.
-- Um 'CasePart' eh composto pela 'clausula do case'
-- (parte dos casamentos) e pela 'clausula do else'
-- (parte executada quando nao ha casamento).
data CasePart =
   CaseClause [CaseMatch] Statement
 | ElseClause Statement
 deriving (Show)

-- | Outra definicao auxiliar para o comando 'case'.
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
-- em ordem crescente de precedencia na linguagem
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
 
 | FunctionCall Identifier [Expr] Int -- Chamada de funcao
 deriving (Show, Eq)
  
-- | Constante. Representa um desses quatro tipos.
data Constant =
   ConstNum Number
 | ConstStr String
 | ConstBool Bool
 | ConstChar Char
 deriving (Show, Eq)


-- * Tabelas

-- | Tabela interna de tipos. Mapeia identificadores para
-- tipos concretos.
--
-- Note que nem todos os tipos utilizados em um programa
-- estarao presentes nessa tabela, pois o HPascal permite
-- declaracao de variaveis com tipos anonimos (sem identificadores).
-- Exemplo: var x : array (3..5) of string;
type TypeTable = Map Identifier Type

-- | Tabela geral de simbolos (variaveis, subrotinas, etc.). Mapeia
-- identificadores para os tipos concretos dos simbolos.
--
-- Devido a declaracao com tipos anonimos, nem todos os tipos presentes
-- aqui estarao na tabela 'TypeTable'.
type SymbolTable = Map Identifier VariableDescriptor

-- | Tabela que mapeia um identificador ao
-- procedimento associado a ele.
type ProcedureTable = Map Identifier Procedure

-- | Tabela que mapeia um identificador a
-- funcao associada a ele.
type FunctionTable  = Map Identifier Function

-- | Descritor de uma variavel. Guarda seu
-- tipo e outras informacoes relevantes.
--
-- Nao guardamos o valor atual da variavel, por exemplo, pois
-- as tabelas deste modulo so serao utilizadas durante o parsing.
data VariableDescriptor = VarDescriptor {
   varType     :: Type
 , isConst     :: Bool
 , isReference :: Maybe Reference
} deriving (Show)

-- | Referencia concreta a uma variavel.
-- Guarda seu escopo e seu identificador.
data Reference = StackReference {
   refScope :: Scope
 , refVar   :: VariableReference
} deriving (Show)

-- | Dados estaticos: um conjunto de tabelas que
-- permite lidar com o ambiente de referenciamento de
-- um determinado bloco de codigo.
data StaticData = StaticData {
    stSymT  :: SymbolTable
  , stTypeT :: TypeTable
  , stProcT :: ProcedureTable
  , stFuncT :: FunctionTable
  , scope   :: Scope
 } deriving (Show)

data VariableReference =
   VarRef Identifier
 | FieldRef VariableReference Identifier
 | IndexRef VariableReference Expr
 deriving (Eq)

baseVarReference :: VariableReference -> Identifier
baseVarReference (VarRef i) = i
baseVarReference (FieldRef r _) = baseVarReference r
baseVarReference (IndexRef r _) = baseVarReference r


instance (Show VariableReference) where
  show (VarRef i)          = i
  show (FieldRef varRef i) = show varRef ++ "." ++ i
  show (IndexRef varRef e) = show varRef ++ "[" ++ show e ++ "]"


data PascalModule = HaskellModule {
    mSymT  :: SymbolTable
  , mTypeT :: TypeTable
  , mProcT :: ProcedureTable
  , mFuncT :: FunctionTable
 } deriving (Show)


-- | Definicao de um procedimento.
data Procedure =
   Procedure   {
     poverloads :: [ProcedureInstance]
   } 
 | HaskellProc {
     pcheck     :: [Type]  -> Bool -- checagem de compatibilidade
   , pfun       :: (MonadIO m) => [Value] -> m ()
   } 

-- | Instancia de um procedimento.
-- Guarda a assinatura da rotina e o bloco associado.
data ProcedureInstance = ProcInstance {
   psignature :: RoutineSignature
 , pcode      :: Block
} deriving (Show)

-- | Definicao de uma funcao.
data Function =
   Function {
     foverloads :: [FunctionInstance]
   }
 | HaskellFunc {
      fcheck     :: [Type] -> Bool -- checagem de compatibilidade
    , ftype      :: Type -- tipo de retorno
    , ffun       :: (MonadIO m) => [Value] -> m Value
   }

-- | Instancia de uma funcao.
-- Guarda a assinatura da rotina, o bloco associado
-- e o tipo de retorno.
data FunctionInstance = FuncInstance {
   fsignature    :: RoutineSignature
 , fcode         :: Block
 , fInstanceType :: Type
} deriving (Show)

-- | Instancia derivada util para exibicao de um procedimento.
instance (Show Procedure) where
  show (Procedure ps) = "Procedure " ++ show ps
  show (HaskellProc _ _) = "HaskellProcedure"

-- | Instancia derivada util para exibicao de uma funcao.
instance (Show Function) where
  show (Function ps)       = "Function " ++ show ps
  show (HaskellFunc _ _ _) = "HaskellFunction"

-- | Instancia derivada util para comparacao
-- de instancias de procedimento.
instance (Eq ProcedureInstance) where
  ProcInstance sig1 _ == ProcInstance sig2 _ = (sig1 == sig2)

-- | Instancia derivada util para comparacao
-- de instancias de funcao.
instance (Eq FunctionInstance) where
  FuncInstance sig1 _ _ == FuncInstance sig2 _ _ = (sig1 == sig2)

-- | Checagem de compatibilidade entre a chamada de um
-- procedimento e sua assinatura.
matchProcCall :: [Type]
              -> [RoutineSignature]
              -> [(RoutineSignature, Int)]
matchProcCall types sigs = match (zip sigs [0..])
  where match [] = []
        match (sigP@(params ,_):ps)
          | match' types params = sigP : match ps
          | otherwise           = match ps
        
        match' [] []                                    =
          True              -- chamadas vazias
        match' []     ((Parameter _ [] _ _):ps)         =
          match' [] ps      -- nao deve vir aqui
        match' []     ((Parameter _ [_] _ (Just _)):ps) =
          match' [] ps      -- usando parametro default
        match' (t:ts) ((Parameter _ [] _ _):ps)         =
          match' (t:ts) ps  -- passando para outra secao
        match' (t:ts) ((Parameter m (_:idl) t' ex):ps)
          | t == t'   =     -- usando argumento
             match' ts ((Parameter m idl t' ex):ps)
          | otherwise =     -- nao casou
             False
        match' _ _ = False
