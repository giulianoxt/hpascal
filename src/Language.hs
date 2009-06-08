-- |Módulo contendo a definição da árvore abstrata
-- de sintaxe do HPascal. Pedaços da árvore são definidos
-- separadamente, usando construtores /data/. 
--
-- Esta árvore não tem como objetivo ser construída
-- /na mão/, e sim pelo parser em "Parsing".
--
-- Note que as definições contidas aqui são semelhantes à
-- gramática presente na documentação final do /HPascal/.

module Language where


-- * Programa


-- |Raiz da árvore, representa um programa inteiro 
data Program = Program Identifier Block
 deriving (Show)


-- * Blocos de código


-- | Bloco de código. Está presente no escopo principal de um programa
-- assim como em definições de /subrotinas/.
--
-- Contêm uma seção de declarações e um 'Statement' (Na verdade, é
-- obrigatoriamente um 'CompoundStatement')
data Block = Block DeclarationPart Statement
 deriving (Show)


-- | Seção de declarações de um 'Block'. Por enquanto, contendo somente
-- uma lista de declarações de variáveis ('VariableDeclaration'), podendo
-- ser vazia.
data DeclarationPart =
  DeclPart [VariableDeclaration]
 deriving (Show)


-- | Declaração de variáveis de um mesmo tipo. 
data VariableDeclaration = VarDec [Identifier] TypeDefinition
 deriving (Show)


-- * Comandos


-- | Representação interna dos comandos aceitos pela linguagem. Um
-- 'Statement' representa um bloco mínimo de execução de código.
data Statement =
    Compound [Statement]                       -- ^ Sequenciamento de comandos
  | Assignment VariableReference AssignOp Expr -- ^ Atribuição
 deriving (Show)


-- * Expressões


-- |Expressões aceitas pela linguagem.
-- Faz uso de construtores prefixos e infixos para melhorar
-- a legibilidade dos casamentos de padrão.
--
-- Os operadores (construtores binários) são apresentados aqui
-- na ordem de precedência da linguagem (menor à maior)
data Expr = 
   Expr :<: Expr         -- ^ Menor que
 | Expr :>: Expr         -- ^ Maior que
 | Expr :=: Expr         -- ^ Igual a
 | Expr :<=: Expr        -- ^ Menor ou igual a
 | Expr :>=: Expr        -- ^ Maior ou igual a
 | Expr :<>: Expr        -- ^ Diferente de
 | Expr `In` Expr        -- ^ Contido em
 
 | Expr :+: Expr         -- ^ Soma
 | Expr :-: Expr         -- ^ Subtração
 | Expr `Or` Expr        -- ^ Ou lógico / booleano
 | Expr `Xor` Expr       -- ^ Xor lógico / booleano
 
 | Expr :*: Expr         -- ^ Multiplicação
 | Expr :/: Expr         -- ^ Divisão (sempre retorna /real/)
 | Expr `Div` Expr       -- ^ Divisão inteira
 | Expr `Mod` Expr       -- ^ Módulo entre inteiros
 | Expr `And` Expr       -- ^ And lógico / booleano
 | Expr `Shl` Expr       -- ^ Deslocamento binário à esquerda
 | Expr `Shr` Expr       -- ^ Deslocamento binário à direita
 
 | Expr :**: Expr        -- ^ Exponenciação
 
 | Not Expr              -- ^ Negação booleana
 | Minus Expr            -- ^ Menos unário
 
 | ConstNum Number       -- ^ Número literal
 | ConstBool Boolean     -- ^ Booleano literal
 | Var VariableReference -- ^ Referência à variável
 deriving (Show)


-- ** Fatores simples


-- | Número literal
-- TODO: extender para ponto-flutuante
type Number = Int

-- | Booleano literal
type Boolean = Bool

-- | Operador de atribuição ('Assignment').
-- Pode ser: "+=", "-=", ":=", "/=", "*="
type AssignOp = String

-- | Identificador presente no programa
type Identifier = String

-- | Definição de tipo.
-- Por enquanto só como um 'Identifier' normal
type TypeDefinition = String

-- | Referência para variável.
-- Por enquanto só como um 'Identifier' normal 
type VariableReference = Identifier
