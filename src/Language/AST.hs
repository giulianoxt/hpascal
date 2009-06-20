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

import Language.Tables
import TypeSystem.Types (Type, Identifier)

import Data.List (intersperse)


-- * Programas

-- | Raiz da arvore, representa um programa inteiro.
data Program = Program Identifier UsesClause Block

instance (Show Program) where
  show = showParseTree

-- | Importam nomes de funcoes e variaveis
-- globais externas para o escopo atual.
data UsesClause = UsesClause [Identifier]
 deriving (Show)

-- | Bloco de codigo. Esta presente no escopo principal de um programa
-- assim como em definicoes de /subrotinas/.
--
-- Contem uma secao de declaracoes e um 'CompoundStatement'.
data Block = Block DeclarationPart Statement StaticData
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
   ProcedureDec Identifier [Parameter] Block
 | FunctionDec  Identifier [Parameter] Identifier Block -- (inclui tipo de retorno)
 deriving (Show)

-- | Declaracao dos parametros para as rotinas.
data Parameter = Parameter PassingMode [Identifier] Type (Maybe Expr)
 deriving (Show)
 
data PassingMode = Value | Const | Reference
 deriving (Show)

-- | Declaracao de arrays.
data ArrayDeclaration = ArrayDec [(Number, Number)]
 deriving (Show)

-- * Comandos

-- | Representacao interna dos comandos aceitos pela linguagem. Um
-- 'Statement' representa um bloco minimo de execucao de codigo.
data Statement =
   -- simple statements
   Nop
 | Assignment String VariableReference Expr
 | ProcedureCall Identifier [Expr]
 
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
 deriving (Show)
 
-- | Por enquanto so como um desses tres tipos.
data Constant =
   ConstNum Number
 | ConstStr String
 | ConstBool Bool
 | ConstChar Char
 deriving (Show)

-- * Fatores simples

type Number = Int

-- | Operador de atribuicao ('Assignment').
-- Pode ser: "+=", "-=", ":=", "/=", "*="
type AssignOp = String

-- | Referencia para variavel.
-- Por enquanto so como um 'Identifier' normal.
type VariableReference = Identifier


showParseTree :: Program -> String
showParseTree = showProgram
 where showProgram (Program name uses block) =
        "Program " ++ show name ++ " "
                   ++ show uses ++ "\n"
                   ++ showBlock 1 block
        
       showBlock n (Block d st _) =
        pad n ++ "Block" ++
        "\n" ++ showDeclPart (n+1) d ++
        "\n" ++ pad n ++ showStmt (n+1) st
       
       showDeclPart n (DeclPart _ _ vds pds) =
        pad n ++ "DeclarationPart" ++
        "\n" ++ showPadList (n+1) vds showVarDecl ++
        "\n" ++ pad n ++ show pds
        
       showVarDecl n (VarDec idl typeV mExpr) =
        pad n ++ "VarDeclaration [" ++ (concat (intersperse " " idl))
        ++ "] " ++ show typeV
        ++ " " ++ "(" ++ show mExpr ++ ")"
       
       showStmt n (Nop) = pad n ++ "Nop"
       
       showStmt n (Assignment op var expr) = pad n ++
        "Assignment " ++ show var ++ " " ++ op ++ " " ++ show expr
       
       showStmt n (Compound stmtl) = pad n ++
        "Compound\n" ++ showPadList (n+1) stmtl showStmt
       
       showStmt n (If expr st1 st2) = pad n ++
        "If " ++ show expr ++ "\n" ++ showStmt (n+1) st1
        ++ "\n" ++ showStmt (n+1) st2
       
       showStmt n (For up ide e1 e2 stmt) = pad n ++
        "For " ++ show up ++ " " ++ show ide ++ " (" ++
        show e1 ++ ") (" ++ show e2 ++ ")\n" ++ showStmt (n+1) stmt
       
       showStmt n (Repeat stmt expr) = pad n ++
        "Repeat " ++ show expr ++ "\n" ++ showStmt (n+1) stmt
       
       showStmt n (While expr stmt) = pad n ++
        "While " ++ show expr ++ "\n" ++ showStmt (n+1) stmt
       
       showStmt n x = pad n ++ show x 

       showPadList n ls f =
        pad n ++ "[" ++ "\n" ++
          (concat (intersperse "\n" (map (f (n+1)) ls)))
          ++ "\n" ++ pad n ++ "]"
          
       pad = (`replicate` ' ')
