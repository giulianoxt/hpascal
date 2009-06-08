-- |Modulo contendo a definicao da arvore abstrata
-- de sintaxe do HPascal. Pedacos da arvore sao definidos
-- separadamente, usando construtores /data/. 
--
-- Esta arvore nao tem como objetivo ser construida
-- /na mao/, e sim pelo parser em "Parsing".
--
-- Note que as definicoes contidas aqui sao semelhantes a
-- gramatica presente na documentacao final do /HPascal/.

module Language where


-- * Programa


-- |Raiz da arvore, representa um programa inteiro 
data Program = Program Identifier Block
 deriving (Show)


-- * Blocos de codigo


-- | Bloco de codigo. Esta presente no escopo principal de um programa
-- assim como em definicoes de /subrotinas/.
--
-- Contem uma secao de declaracoes e um 'Statement' (Na verdade, e
-- obrigatoriamente um 'CompoundStatement')
data Block = Block DeclarationPart Statement
 deriving (Show)


-- | Secao de declaracoes de um 'Block'. Por enquanto, contendo somente
-- uma lista de declaracoes de variaveis ('VariableDeclaration'), podendo
-- ser vazia.
data DeclarationPart =
  DeclPart [VariableDeclaration]
 deriving (Show)


-- | Declaracao de variaveis de um mesmo tipo. 
data VariableDeclaration = VarDec [Identifier] TypeDefinition
 deriving (Show)


-- * Comandos


-- | Representacao interna dos comandos aceitos pela linguagem. Um
-- 'Statement' representa um bloco minimo de execucao de codigo.
data Statement =
    Compound [Statement]                       -- ^ Sequenciamento de comandos
  | Assignment VariableReference AssignOp Expr -- ^ Atribuicao
 deriving (Show)


-- * Expressoes


-- |Expressoes aceitas pela linguagem.
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
 
 | ConstNum Number       -- ^ Número literal
 | ConstBool Boolean     -- ^ Booleano literal
 | Var VariableReference -- ^ Referencia a variavel
 deriving (Show)


-- ** Fatores simples


-- | Número literal
-- TODO: extender para ponto-flutuante
type Number = Int

-- | Booleano literal
type Boolean = Bool

-- | Operador de atribuicao ('Assignment').
-- Pode ser: "+=", "-=", ":=", "/=", "*="
type AssignOp = String

-- | Identificador presente no programa
type Identifier = String

-- | Definicao de tipo.
-- Por enquanto so como um 'Identifier' normal
type TypeDefinition = String

-- | Referencia para variavel.
-- Por enquanto so como um 'Identifier' normal 
type VariableReference = Identifier
