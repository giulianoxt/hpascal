-- HParser
module Parsing where

import Language
import TypeChecker
import qualified Token as Tk
import ParserRun (HParser, ParserState)

import Data.Char (isLetter)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


hparser :: HParser (Program, ParserState)
hparser = do Tk.whiteSpace
             p <- program
             st <- getState
             return (p, st)


program :: HParser Program
program = 
  do Tk.reserved "program"
     i <- Tk.identifier
     Tk.symbol ";"
     b <- block
     Tk.symbol "."
     return (Program i b)


block :: HParser Block
block = 
  do decl <- declarations
     processDeclPart decl
     stat <- compoundStmt
     return (Block decl stat)


declarations :: HParser DeclarationPart
declarations = 
  do {
     Tk.reserved "var";
     varl <- Tk.semiSep1 varDeclaration;
     return (DeclPart varl)
  } <|>
    (return (DeclPart []))


varDeclaration :: HParser VariableDeclaration
varDeclaration =
  do varIdL  <- Tk.commaSep1 Tk.identifier
     Tk.symbol ":"
     typeId <- Tk.identifier
     return (VarDec varIdL typeId)


statement :: HParser Statement
statement = 
     assignmentStmt
 <|> compoundStmt


assignmentStmt :: HParser Statement
assignmentStmt =
  do varRef <- variableReference
     op     <- assignOp
     expr   <- expression
     return (Assignment varRef op expr)
 where assignOp :: HParser String
       assignOp = do op <- oneOf ":+-*/"
                     eq <- Tk.symbol "="
                     return (op : eq)


compoundStmt :: HParser Statement
compoundStmt =
  do Tk.reserved "begin"
     stmtl <- option [] stmtSeq
     Tk.reserved "end"
     return (Compound stmtl)
 where stmtSeq :: HParser [Statement]
       stmtSeq = do l <- Tk.semiSep1 statement
                    option "" (Tk.symbol ";")
                    return l


variableReference :: HParser Identifier
variableReference = Tk.identifier


expression :: HParser Expr
expression = buildExpressionParser operators simpleExpr


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


simpleExpr :: HParser Expr
simpleExpr = 
     constBoolean
 <|> constNumber
 <|> Tk.parens expression
 <|> Var `liftM` Tk.identifier


constBoolean :: HParser Expr
constBoolean =
  do b <- (Tk.reserved "true"  >> return True) <|>
          (Tk.reserved "false" >> return False)
     return (ConstBool b)


constNumber :: HParser Expr
constNumber =
   do Left n <- Tk.number
      return (ConstNum (fromIntegral n))

