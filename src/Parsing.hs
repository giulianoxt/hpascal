-- | Parser completo do HPascal
-- Montado com base na biblioteca Parsec, e um parser
-- combinatorio, monadico.
-- 
-- O parser principal e o 'hparser', que faz uso de
-- todos os parsers abaixo. Cada parser reconhece parte
-- do input, e retorna um pedaco da arvore do programa 

module Parsing where

import Language
import TypeChecker
import qualified Token as Tk
import ParsingState (HParser, ParserState)

import Data.Char (isLetter)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


-- | Parser principal. Retorna a arvore de parsing
-- junto com o estado final do parser.
hparser :: HParser (Program, ParserState)
hparser = do Tk.whiteSpace
             p <- program
             eof
             st <- getState
             return (p, st)


-- | Parser de um programa completo ('Program'). Reconhece o identificador
-- de nome e um 'block'.
program :: HParser Program
program = 
  do Tk.reserved "program"
     i <- Tk.identifier
     Tk.symbol ";"
     b <- block
     Tk.symbol "."
     return (Program i b)


-- | Reconhece um bloco de codigo ('Block'), contendo declaracoes
-- e uma sequencia de statements.
block :: HParser Block
block = 
  do decl <- declarations
     stat <- compoundStmt
     return (Block decl stat)


-- | Secao de declaracoes. E composta de varias sub-secoes,
-- comecando com keywords e contendo uma outra lista de declaracoes.
declarations :: HParser DeclarationPart
declarations =
  do varL <- optSection "var" varDeclaration 
     return (DeclPart varL)

 where -- | Recebe uma palavra chave e um parser para uma declaracao,
       -- e monta um parser para uma secao opcional de declaracoes
       optSection :: String -> HParser a -> HParser [a]
       optSection keyword p =
        (Tk.reserved keyword >> Tk.semiSep1 p) <|> (return [])


-- | Declaracao de uma variavel. Reconhece uma lista de identificadores
-- seguida de um tipo. Apos montar o trecho da arvore ('VarDec')
--
-- Apos montar o trecho da arvore com 'VarDec', chama 'processVarDecl'
-- para processar a secao de declaracoes (atualizar tabela de simbolos,
-- reportar erros, etc.)
varDeclaration :: HParser VariableDeclaration
varDeclaration =
  do varIdl  <- Tk.commaSep1 Tk.identifier
     Tk.symbol ":"
     typeId <- Tk.identifier
     let varDec = VarDec varIdl typeId
     processVarDecl varDec
     return varDec
 <?> "variable declaration"


-- | Parser completo de comandos.
statement :: HParser Statement
statement = 
     assignmentStmt
 <|> compoundStmt
 <?> "statement"


-- | Atribuicoes. Reconhece atribuicoes simples
-- e compostas, com expressoes do lado direito e 
-- referencias para variaveis do lado esquerdo.
assignmentStmt :: HParser Statement
assignmentStmt =
  do varRef <- variableReference
     op     <- assignOp
     expr   <- expression
     let assign = Assignment varRef op expr 
     processAssignment assign
     return assign
 where assignOp :: HParser String
       assignOp = do op <- oneOf ":+-*/"
                     eq <- Tk.symbol "="
                     return (op : eq)


-- | Sequenciamento de comandos, utilizando um bloco
-- /begin/ e /end/. 
compoundStmt :: HParser Statement
compoundStmt =
  do Tk.reserved "begin"
     stmtl <- option [] stmtSeq
     Tk.reserved "end"
     return (Compound stmtl)
 where stmtSeq :: HParser [Statement]
       stmtSeq = Tk.semiSep1 statement


-- | Referencia para variavel, por enquanto so um identifier.
variableReference :: HParser Identifier
variableReference = Tk.identifier


-- | Parser completo de expressoes, construido utilizando o
-- 'buildExpressionParser'. 
expression :: HParser Expr
expression = buildExpressionParser operators simpleExpr


-- | Tabela de operadores para o 'buildExpressionParser',
-- em ordem precedencia e com informacoes de associatividade
operators :: [[Operator Char ParserState Expr]]
operators =
  [ [Infix (parseOp "**" (:**:)) AssocRight],
    
    [unary "not" Not, unary "+" id, unary "-" Minus],
  
    [binary "*" (:*:), binary "/" (:/:), binary "div" Div,
     binary "mod" Mod, binary "and" And, binary "shl" Shl, binary "shr" Shr],
    
    [binary "+" (:+:), binary "-" (:-:), binary "or" Or, binary "xor" Xor],
    
    [binary "<>" (:<>:), binary "<=" (:<=:), binary "<" (:<:), binary ">=" (:>=:),
     binary ">" (:>:), binary "=" (:=:), binary "in" In]
  ]
 where unary  name f  = Prefix (parseOp name f)
       binary name f  = Infix  (parseOp name f) AssocLeft
       parseOp name f =
         Tk.lexeme $ try $
         do { string name;
              if literal name then notFollowedBy (letter <|> char '_' <|> digit)
                              else return ();
              return f
          }
       literal = isLetter . head


-- | Parser para expressoes simples (sem operadores) 
simpleExpr :: HParser Expr
simpleExpr = 
     constBoolean
 <|> constNumber
 <|> Tk.parens expression
 <|> Var `liftM` Tk.identifier
 <?> "expression"


-- | Booleano literal
constBoolean :: HParser Expr
constBoolean =
  do b <- (Tk.reserved "true"  >> return True) <|>
          (Tk.reserved "false" >> return False)
     return (ConstBool b)


-- | Numero literal
constNumber :: HParser Expr
constNumber =
   do Left n <- Tk.number
      return (ConstNum (fromIntegral n))
