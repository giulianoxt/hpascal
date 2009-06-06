-- Arvore que representa os programas HPascal

module Language where

data Program = Program Identifier Block
 deriving (Show)

data Block = Block DeclarationPart Statement
 deriving (Show)

data DeclarationPart =
  DeclPart [VariableDeclaration]
 deriving (Show)

data VariableDeclaration = VarDec [Identifier] TypeDefinition
 deriving (Show)

data Statement =
    Compound [Statement]
  | Assignment VariableReference AssignOp Expr 
 deriving (Show)

data Expr = 
   Expr :<: Expr
 | Expr :>: Expr
 | Expr :=: Expr
 | Expr :<=: Expr
 | Expr :>=: Expr
 | Expr :<>: Expr
 | Expr `In` Expr
 
 | Expr :+: Expr
 | Expr :-: Expr
 | Expr `Or` Expr
 | Expr `Xor` Expr
 
 | Expr :*: Expr
 | Expr :/: Expr
 | Expr `Div` Expr
 | Expr `Mod` Expr
 | Expr `And` Expr
 | Expr `Shl` Expr
 | Expr `Shr` Expr
 
 | Expr :**: Expr
 
 | Not Expr
 | Minus Expr
 
 | ConstNum Number
 | ConstBool Boolean
 | Var VariableReference
 deriving (Show)


type Number = Int
type Boolean = Bool
type AssignOp = String
type Identifier = String
type TypeDefinition = String
type VariableReference = Identifier
