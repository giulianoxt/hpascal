-- | Parser completo do HPascal
-- Montado com base na biblioteca Parsec, e um parser
-- combinatorio, monadico.
-- 
-- O parser principal e o 'hparser', que faz uso de
-- todos os parsers abaixo. Cada parser reconhece parte
-- do input, e retorna um pedaco da arvore do programa 

module Parser.HParser where

import Language.AST
import Language.Basic
import TypeSystem.Checker
import TypeSystem.Types hiding (fail)

import Parser.State
import qualified Parser.Tokens as T

import Data.Map (fromList)
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
     ident   <- T.identifier
     T.symbol ";"
     
     imports <- option (UsesClause []) usesClause
     processImports imports
     sd <- getHeadStaticData
     
     b       <- enterBlock ident
     T.symbol "."
     return (Program ident imports b sd)
 where
  usesClause :: HParser UsesClause
  usesClause =
    do T.reserved "uses"
       idl <- T.commaSep1 T.identifier
       T.symbol ";"
       return $ UsesClause idl


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
  do constL <- optSection constDeclarations
     typeL  <- optSection typeDeclarations
     varL   <- optSection varDeclarations
     rdecls <- T.semiSep routineDecl
     
     return (DeclPart constL typeL varL rdecls)

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
     
           mexpr <- option Nothing $ T.symbol "=" >> liftM Just expression
     
           let dec = VarDec varIdl typeV mexpr
     
           processVarDecl dec
           return dec


constDeclarations :: HParser [ConstantDeclaration]
constDeclarations = liftM concat (many1 constDeclaration)
 where constDeclaration =
        do T.reserved "const"
           T.semiSep1 singleConstDecl
        <?> "constant declaration"
       
       singleConstDecl =
        do varId  <- T.identifier
          
           typeV  <- option Nothing $ liftM Just $
                        do T.symbol ":"
                           parseIdentifierType
           
           T.symbol "="
           expr   <- expression
           
           let dec = ConstDec varId typeV expr
           
           processConstDecl dec
           return dec


typeDeclarations :: HParser [TypeDeclaration]
typeDeclarations = liftM concat (many1 typeDeclaration)
 where typeDeclaration = 
        do T.reserved "type"
           T.semiSep1 singleTypeDecl
        <?> "type definition"
       
       singleTypeDecl =
        do typeId <- T.identifier
           T.symbol "="
           typeV <- parseType            
           
           let typeDecl = TypeDec typeId typeV
           processTypeDecl typeDecl
           return typeDecl


routineDecl :: HParser RoutineDeclaration
routineDecl =
     functionDecl
 <|> procedureDecl
 <?> "routine declaration"


procedureDecl :: HParser RoutineDeclaration
procedureDecl =
  do T.reserved "procedure"
     ident  <- T.identifier
     params <- T.parens (T.semiSep parameter)
     T.symbol ";"
     bl     <- enterProcedureBlock ident params
     
     return $ ProcedureDec ident params bl


functionDecl :: HParser RoutineDeclaration
functionDecl =
  do T.reserved "function"
     ident  <- T.identifier
     params <- T.parens (T.semiSep parameter)
     T.symbol ":"
     typeId <- parseIdentifierType
     T.symbol ";"
     bl     <- enterFunctionBlock ident typeId params
     
     return $ FunctionDec ident params typeId bl


parameter :: HParser Parameter
parameter =
 do mode   <- ((T.reserved "const" >> return Const)     <|>
               (T.reserved "var"   >> return Reference) <|>
                return Value)

    identL  <- T.commaSep1 T.identifier
    T.symbol ":"
    typeV  <- parseType
    
    defV <- option Nothing $ T.symbol "=" >>
                            Just `liftM` expression
    
    return $ Parameter mode identL typeV defV


-- | Faz parsing de uma definição de tipo, retornando um
-- tipo concreto.
--
-- Faz um lookup na tabela interna. Caso o tipo ainda nao
-- tenha sido definido, retorna um UnknownType e loga o erro
-- no estado interno do parser.
parseType :: HParser Type
parseType =
     parseArrayType
 <|> parseRecordType
 <|> parseIdentifierType
 <?> "type declaration"  
 
parseIdentifierType :: HParser Type
parseIdentifierType = 
  do typeId <- T.identifier
     getTypeVal typeId

parseArrayType :: HParser Type
parseArrayType =
  do T.reserved "array"
     dimensions <- T.brackets $ T.commaSep1 $ arrayRange
     T.reserved "of"
     typeV      <- parseIdentifierType
     
     numDimensions <- mapM evalDimension dimensions
     
     return $ makeArrayV numDimensions typeV 
 where
  arrayRange :: HParser (Expr, Expr)
  arrayRange =
    do c <- expression
       option (zero, c) $ do T.symbol ".."
                             c' <- expression
                             return (c, c')
  
  makeArrayV :: [(Int, Int)] -> Type -> Type
  makeArrayV [] t     = t
  makeArrayV (d:ds) t = ArrayT d (makeArrayV ds t)
  
  evalDimension :: (Expr, Expr) -> HParser (Int, Int)
  evalDimension (a,b) =
    do a' <- evalConstant a
       b' <- evalConstant b
       return (a', b')
  
  zero :: Expr
  zero = ConstExpr $ ConstNum $ IntNum $ 0

parseRecordType :: HParser Type
parseRecordType =
  do T.reserved "record"
     fieldL <- liftM concat (T.semiSep field)
     T.reserved "end"
     
     return $ RecordT (fromList fieldL)
 where
  field :: HParser [(Identifier, Type)]
  field = 
    do idl   <- T.commaSep1 T.identifier
       T.symbol ":"
       typeV <- parseType
       return [(ident, typeV) | ident <- idl]


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
    
     ( procedureCallStmt varRef <|>
       assignmentStmt    varRef <|>
       specialProcedureCall varRef [] )


procedureCallStmt :: VariableReference -> HParser Statement
procedureCallStmt varRef =
  do params <- T.parens (T.commaSep expression)
     specialProcedureCall varRef params


specialProcedureCall :: VariableReference -> [Expr] -> HParser Statement
specialProcedureCall (VarRef varId) params =
  do 
     let call = ProcedureCall varId params (-1)
     call' <- processProcCall call
     return call'
specialProcedureCall _ _ = fail $ "procedure call with "
                               ++ "compound reference"

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
     
     -- Pode mudar para um functionReturn
     stmt <- processAssignment assign
     
     return stmt
     
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
     varRef  <- variableReference
     T.symbol ":="
     
     expr1  <- expression
     checkOrdinalExpr expr1
     checkAssignExpr varRef expr1
     
     update <-  (T.reserved "to"     >> return To)
            <|> (T.reserved "downto" >> return Downto)   
            
     expr2  <- expression
     checkOrdinalExpr expr2
     checkAssignExpr varRef expr2

     T.reserved "do"
     stmt   <- statement
     
     return (For update varRef expr1 expr2 stmt)


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
variableReference :: HParser VariableReference
variableReference =
  do ident <- T.identifier
     varReference (VarRef ident)
 where
  -- Simula a recursao a esquerda da gramatica
  varReference varRef =
       fieldReference varRef
   <|> indexReference varRef
   <|> return varRef
  
  fieldReference varRef =
    do T.symbol "."
       field <- T.identifier
       varReference $ FieldRef varRef field
  
  indexReference varRef =
    do exprL <- T.brackets $ T.commaSep1 $ expression
       varReference $ buildIndexRef varRef (reverse exprL)
  
  buildIndexRef varRef []     = varRef
  buildIndexRef varRef (e:es) = IndexRef (buildIndexRef varRef es) e
      


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
 <|> identifierExpr
 <|> ConstExpr `liftM` constant
 <?> "expression"


constant :: HParser Constant
constant =
     ConstNum  `liftM` constNumber
 <|> ConstBool `liftM` constBoolean
 <|> ConstStr  `liftM` T.stringLiteral
 <|> ConstChar `liftM` T.charLiteral


-- | Booleano literal
constBoolean :: HParser Bool
constBoolean =
     (T.reserved "true"  >> return True)
 <|> (T.reserved "false" >> return False)


-- | Numero literal
constNumber :: HParser Number
constNumber =
   do num <- T.number
      return $ case num of
                 Left  n -> IntNum (fromIntegral n)
                 Right n -> FloatNum (realToFrac n)

intNumber :: HParser Int
intNumber = 
  do Left n <- T.number
     return $ fromIntegral n


identifierExpr :: HParser Expr
identifierExpr =
  do varRef <- variableReference
     option (Var varRef) (functionCall varRef)


functionCall :: VariableReference -> HParser Expr
functionCall (VarRef funcId) =
  do params <- T.parens (T.commaSep expression)
     let call = FunctionCall funcId params (-1)
     call'  <- processFuncCall call
     return call'
functionCall _ =
 do T.symbol "("
    fail $ "function call with " ++ "compound var reference"


enterBlock :: Identifier -> HParser Block
enterBlock ident =
  liftM fst $ withNewScope ident (False, error "X") block


enterProcedureBlock :: Identifier
                    -> [Parameter]
                    -> HParser Block
enterProcedureBlock ident params =
 do updateProcT False undefined $ ProcedureDec ident params undefined
  
    (b,sd') <- withNewScope ident (False, error "X") $
                  do processParams params
                     block

    updateProcT True sd' $ ProcedureDec ident params b
    
    return b


enterFunctionBlock :: Identifier
                   -> Type
                   -> [Parameter]
                   -> HParser Block
enterFunctionBlock ident typeId params =
  do updateFuncT False undefined $ FunctionDec ident params typeId undefined
    
     (b,sd') <- withNewScope ident (True,typeId) $
                  do processParams params
                     block
                                        
     updateFuncT True sd' $ FunctionDec ident params typeId b

     return b
