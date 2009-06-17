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
import Types (Type)
import qualified Token as Tk
import ParsingState (HParser, ParserState, initialState)

import Data.Char (isLetter)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


-- | Funcao para execucao do parser principal do HPascal
parse :: SourceName
      -> String
      -> Either ParseError (Program, ParserState)
parse src input = runParser hparser initialState src input


-- | Parser principal. Retorna a arvore de parsing
-- junto com o estado final do parser.
hparser :: HParser (Program, ParserState)
hparser = do Tk.whiteSpace
             p  <- program
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
     return (Program i (UsesClause []) b)


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
  do varL     <- optSection varDeclarations
     procDecl <- Tk.semiSep procedureDecl
     
     return (DeclPart [] [] varL procDecl)

 where -- | Recebe um parser para uma declaracao,
       -- e monta um parser para uma secao opcional de declaracoes
       optSection :: HParser [a] -> HParser [a]
       optSection p = option [] $ liftM concat (Tk.semiSep1 p)


-- | Declaracao de uma variavel. Reconhece uma lista de identificadores
-- seguida de um tipo. Apos montar o trecho da arvore ('VarDec')
--
-- Apos montar o trecho da arvore com 'VarDec', chama 'processVarDecl'
-- para processar a secao de declaracoes (atualizar tabela de simbolos,
-- reportar erros, etc.)
varDeclarations :: HParser [VariableDeclaration]
varDeclarations = liftM concat (many1 varDeclaration)
 where varDeclaration = 
        do Tk.reserved "var"
           Tk.semiSep1 singleVarDecl
        <?> "variable declaration"
       
       singleVarDecl =
        do varIdl <- Tk.commaSep1 Tk.identifier
           Tk.symbol ":"
           typeV  <- parseType
     
           mexpr <- (Tk.symbol "=" >> liftM Just expression)
                 <|> return Nothing
     
           let dec = VarDec varIdl typeV mexpr
     
           processVarDecl dec
           return dec


procedureDecl :: HParser RoutineDeclaration
procedureDecl =
  do Tk.reserved "procedure"
     ident  <- Tk.identifier
     params <- Tk.parens (Tk.commaSep1 parameter)
     Tk.symbol ";"
     bl     <- block
     return (ProcedureDec ident params bl)
 
 where parameter :: HParser Parameter
       parameter =
        do mode   <- ((Tk.reserved "const" >> return Const)     <|>
                      (Tk.reserved "var"   >> return Reference) <|>
                       return Value)

           identL <- Tk.commaSep1 Tk.identifier
           Tk.symbol ":"
           typeV  <- parseType
           
           defV <- option Nothing (Tk.symbol "=" >>
                                   Just `liftM` expression)
           
           return (Parameter mode identL typeV defV)


-- | Faz parsing de uma definição de tipo, retornando um
-- tipo concreto.
--
-- Faz um lookup na tabela interna. Caso o tipo ainda nao
-- tenha sido definido, retorna um UnknownType e loga o erro
-- no estado interno do parser.
parseType :: HParser Type
parseType =
  do typeId <- Tk.identifier
     getTypeVal typeId


-- | Parser completo de comandos.
statement :: HParser Statement
statement = 
     ifStmt
 <|> forStmt
 <|> caseStmt
 <|> whileStmt
 <|> repeatStmt
 <|> compoundStmt
 <|> identifierStmt
 <?> "statement"


statementSeq :: HParser [Statement]
statementSeq = Tk.semiSep statement


identifierStmt :: HParser Statement
identifierStmt =
  do varRef <- variableReference
    
     ( (lookAhead (Tk.symbol "(") >> procedureCallStmt varRef) <|>
       assignmentStmt varRef)


procedureCallStmt :: VariableReference -> HParser Statement
procedureCallStmt varRef =
  do params <- Tk.parens (Tk.commaSep expression)
     return (ProcedureCall varRef params)


-- | Atribuicoes. Reconhece atribuicoes simples
-- e compostas, com expressoes do lado direito e 
-- referencias para variaveis do lado esquerdo.
assignmentStmt :: VariableReference -> HParser Statement
assignmentStmt varRef =
  do op   <- assignOp
     expr <- expression
     
     let assign = Assignment op varRef expr 
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
     stmtl <- statementSeq
     Tk.reserved "end"
     
     return (Compound stmtl)


ifStmt :: HParser Statement
ifStmt =
  do Tk.reserved "if"
    
     expr <- expression
     processBooleanExpr expr
     
     Tk.reserved "then"
     st1  <- statement
     st2  <- (Tk.reserved "else" >> statement)
         <|> return Nop
         
     return (If expr st1 st2)


forStmt :: HParser Statement
forStmt =
  do Tk.reserved "for"
     varId  <- Tk.identifier
     Tk.symbol ":="
     
     expr1  <- expression
     processAssignment (Assignment ":=" varId expr1)
     
     update <-  (Tk.reserved "to"     >> return To)
            <|> (Tk.reserved "downto" >> return Downto)   
            
     expr2  <- expression
     processAssignment (Assignment ":=" varId expr2)

     Tk.reserved "do"
     stmt   <- statement
     
     return (For update varId expr1 expr2 stmt)


whileStmt :: HParser Statement
whileStmt =
  do Tk.reserved "while"
    
     expr <- expression
     processBooleanExpr expr
     
     Tk.reserved "do"
     stmt <- statement
     return (While expr stmt)


caseStmt :: HParser Statement
caseStmt =
  do Tk.reserved "case"
     expr  <- expression
     Tk.reserved "of"
     
     cps   <- Tk.semiSep1 casePart
     elsep <- option [] elsePart
       
     Tk.reserved "end"
     
     return (Case expr (cps ++ elsep))
     
 where casePart  = do matchs <- Tk.commaSep1 caseMatch
                      Tk.reserved ":"
                      stmt   <- statement
                      return (CaseClause matchs stmt)

       caseMatch = do c1 <- constant
                      ((Tk.symbol ".." >> constant >>= return.(RangeCase c1))
                       <|> return (SingleCase c1))

       elsePart  = do Tk.reserved "else"
                      stmt <- statement
                      option "" Tk.semi
                      return [ElseClause stmt]


repeatStmt :: HParser Statement
repeatStmt =
  do Tk.reserved "repeat"
     stmtl <- statementSeq
     Tk.reserved "until"
     
     expr  <- expression
     processBooleanExpr expr
     
     return (Repeat (Compound stmtl) expr)


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
     Tk.parens expression
 <|> Var       `liftM` Tk.identifier
 <|> ConstExpr `liftM` constant
 <?> "expression"


constant :: HParser Constant
constant =
     ConstNum  `liftM` constNumber
 <|> ConstBool `liftM` constBoolean


-- | Booleano literal
constBoolean :: HParser Bool
constBoolean =
     (Tk.reserved "true"  >> return True)
 <|> (Tk.reserved "false" >> return False)


-- | Numero literal
constNumber :: HParser Int
constNumber =
   do Left n <- Tk.number
      return (fromIntegral n)
