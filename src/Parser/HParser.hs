-- | Parser completo do HPascal
-- Montado com base na biblioteca Parsec, e um parser
-- combinatorio, monadico.
-- 
-- O parser principal e o 'hparser', que faz uso de
-- todos os parsers abaixo. Cada parser reconhece parte
-- do input, e retorna um pedaco da arvore do programa 

module Parser.HParser where

import Language.AST
import TypeSystem.Checker
import TypeSystem.Types (Type, Identifier)

import Parser.State
import qualified Parser.Tokens as T

import Data.Char (isLetter)
import Control.Monad (when, liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


-- | Funcao para execucao do parser principal do HPascal
parse :: SourceName
      -> String
      -> Either String Program
parse src input =
  case result of
    Left parseErr -> Left ("Parse Error:\n" ++ show parseErr)
    Right (p, st) -> case compErrors st of
                      Nothing -> Right p
                      Just s  -> Left s
 where
  result = runParser hparser initialState src input
  


-- | Parser principal. Retorna a arvore de parsing
-- junto com o estado final do parser.
hparser :: HParser (Program, ParserState)
hparser = do T.whiteSpace
             p  <- program
             eof
             st <- getState
             return (p, st)


-- | Parser de um programa completo ('Program'). Reconhece o identificador
-- de nome e um 'block'.
program :: HParser Program
program = 
  do T.reserved "program"
     ident <- T.identifier
     T.symbol ";"
     b     <- enterBlock ident
     T.symbol "."
     return (Program ident (UsesClause []) b)


-- | Reconhece um bloco de codigo ('Block'), contendo declaracoes
-- e uma sequencia de statements.
block :: HParser Block
block = 
  do decl   <- declarations
     stmt   <- compoundStmt
     stData <- getStaticData
     return (Block decl stmt stData)


-- | Secao de declaracoes. E composta de varias sub-secoes,
-- comecando com keywords e contendo uma outra lista de declaracoes.
declarations :: HParser DeclarationPart
declarations =
  do varL      <- optSection varDeclarations
     procDecls <- T.semiSep procedureDecl
     
     return (DeclPart [] [] varL procDecls)

 where -- | Recebe um parser para uma declaracao,
       -- e monta um parser para uma secao opcional de declaracoes
       optSection :: HParser [a] -> HParser [a]
       optSection p = option [] $ liftM concat (T.semiSep1 p)


-- | Declaracao de uma variavel. Reconhece uma lista de identificadores
-- seguida de um tipo. Apos montar o trecho da arvore ('VarDec')
--
-- Apos montar o trecho da arvore com 'VarDec', chama 'processVarDecl'
-- para processar a secao de declaracoes (atualizar tabela de simbolos,
-- reportar erros, etc.)
varDeclarations :: HParser [VariableDeclaration]
varDeclarations = liftM concat (many1 varDeclaration)
 where varDeclaration = 
        do T.reserved "var"
           T.semiSep1 singleVarDecl
        <?> "variable declaration"
       
       singleVarDecl =
        do varIdl <- T.commaSep1 T.identifier
           T.symbol ":"
           typeV  <- parseType
     
           mexpr <- (T.symbol "=" >> liftM Just expression)
                 <|> return Nothing
     
           let dec = VarDec varIdl typeV mexpr
     
           processVarDecl dec
           return dec


procedureDecl :: HParser RoutineDeclaration
procedureDecl =
  do T.reserved "procedure"
     ident  <- T.identifier
     params <- T.parens (T.semiSep parameter)
     T.symbol ";"
     bl     <- enterProcedureBlock ident params
     return (ProcedureDec ident params bl)
 
 where parameter :: HParser Parameter
       parameter =
        do mode   <- ((T.reserved "const" >> return Const)     <|>
                      (T.reserved "var"   >> return Reference) <|>
                       return Value)

           identL <- T.commaSep1 T.identifier
           T.symbol ":"
           typeV  <- parseType
           
           defV <- option Nothing $ T.symbol "=" >>
                                    Just `liftM` expression
           
           return (Parameter mode identL typeV defV)


-- | Faz parsing de uma definição de tipo, retornando um
-- tipo concreto.
--
-- Faz um lookup na tabela interna. Caso o tipo ainda nao
-- tenha sido definido, retorna um UnknownType e loga o erro
-- no estado interno do parser.
parseType :: HParser Type
parseType =
  do typeId <- T.identifier
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
statementSeq = T.semiSep statement


identifierStmt :: HParser Statement
identifierStmt =
  do varRef <- variableReference
    
     ( (lookAhead (T.symbol "(") >> procedureCallStmt varRef) <|>
       assignmentStmt varRef )


procedureCallStmt :: VariableReference -> HParser Statement
procedureCallStmt varRef =
  do params <- T.parens (T.commaSep expression)
     return (ProcedureCall varRef params)


-- | Atribuicoes. Reconhece atribuicoes simples
-- e compostas, com expressoes do lado direito e 
-- referencias para variaveis do lado esquerdo.
assignmentStmt :: VariableReference -> HParser Statement
assignmentStmt varRef =
  do op   <- assignOp
     expr <- expression
     
     let rhs = case head op of
                '+' -> (varExpr :+:)
                '-' -> (varExpr :-:)
                '*' -> (varExpr :*:)
                '/' -> (varExpr :/:)
                ':' -> id
                _   -> error "HParser.assignmentStmt"
                
         assign = Assignment varRef (rhs expr) 
     
     processAssignment assign
     return assign
     
 where assignOp :: HParser String
       assignOp = do op <- oneOf ":+-*/"
                     eq <- T.symbol "="
                     return (op : eq)
       
       varExpr :: Expr
       varExpr = Var varRef


-- | Sequenciamento de comandos, utilizando um bloco
-- /begin/ e /end/. 
compoundStmt :: HParser Statement
compoundStmt =
  do T.reserved "begin"
     stmtl <- statementSeq
     T.reserved "end"
     
     return (Compound stmtl)


ifStmt :: HParser Statement
ifStmt =
  do T.reserved "if"
    
     expr <- expression
     checkBooleanExpr expr
     
     T.reserved "then"
     st1  <- statement
     st2  <- option Nop $ T.reserved "else" >> statement
         
     return (If expr st1 st2)


forStmt :: HParser Statement
forStmt =
  do T.reserved "for"
     varId  <- T.identifier
     T.symbol ":="
     
     expr1  <- expression
     checkAssignExpr varId expr1
     
     update <-  (T.reserved "to"     >> return To)
            <|> (T.reserved "downto" >> return Downto)   
            
     expr2  <- expression
     checkAssignExpr varId expr2

     T.reserved "do"
     stmt   <- statement
     
     return (For update varId expr1 expr2 stmt)


whileStmt :: HParser Statement
whileStmt =
  do T.reserved "while"
    
     expr <- expression
     checkBooleanExpr expr
     
     T.reserved "do"
     stmt <- statement
     return (While expr stmt)


caseStmt :: HParser Statement
caseStmt =
  do T.reserved "case"
     expr  <- expression
     T.reserved "of"
     
     cps   <- T.semiSep1 (casePart expr)
     elsep <- option [] elsePart
       
     T.reserved "end"
     
     return (Case expr (cps ++ elsep))
     
 where casePart e  = do matchs <- T.commaSep1 (caseMatch e)
                        T.reserved ":"
                        stmt   <- statement
                        return (CaseClause matchs stmt)

       caseMatch e = do c1    <- constant
                        match <- option (SingleCase c1) $
                                  do T.symbol ".."
                                     c2 <- constant
                                     return (RangeCase c1 c2)
                        checkCaseMatch e match
                        return match

       elsePart    = do T.reserved "else"
                        stmt <- statement
                        option "" T.semi
                        return [ElseClause stmt]


repeatStmt :: HParser Statement
repeatStmt =
  do T.reserved "repeat"
     stmtl <- statementSeq
     T.reserved "until"
     
     expr  <- expression
     checkBooleanExpr expr
     
     return (Repeat (Compound stmtl) expr)


-- | Referencia para variavel, por enquanto so um identifier.
variableReference :: HParser Identifier
variableReference = T.identifier


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
 where 
       unary  name f  = Prefix (parseOp name f)

       binary name f  = Infix  (parseOp name f) AssocLeft
       
       parseOp name f = T.lexeme $ try $
                         do string name
                            when (literal name) $ notFollowedBy $
                              letter <|> char '_' <|> digit
                            return f

       literal = isLetter . head


-- | Parser para expressoes simples (sem operadores) 
simpleExpr :: HParser Expr
simpleExpr = 
     T.parens expression
 <|> Var       `liftM` T.identifier
 <|> ConstExpr `liftM` constant
 <?> "expression"


constant :: HParser Constant
constant =
     ConstNum  `liftM` constNumber
 <|> ConstBool `liftM` constBoolean
 <|> ConstStr  `liftM` T.stringLiteral


-- | Booleano literal
constBoolean :: HParser Bool
constBoolean =
     (T.reserved "true"  >> return True)
 <|> (T.reserved "false" >> return False)


-- | Numero literal
constNumber :: HParser Int
constNumber =
   do Left n <- T.number
      return (fromIntegral n)


enterBlock :: Identifier -> HParser Block
enterBlock ident =
  withNewScope ident block

enterProcedureBlock :: Identifier -> [Parameter] -> HParser Block
enterProcedureBlock ident params =
  withNewScope ident (processParams params >> block)
