-- Definições dos parsers para os 
-- diversos lexemas do HPascal
-- usa as facilidades providas pelo Parsec.Token
module Token where

import Language
import ParserRun

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T


hPascalDef   :: LanguageDef ParserState
hPascalDef = emptyDef {
    commentStart   = "{"
  , commentEnd     = "}"
  , commentLine    = "//"
  , nestedComments = True
  , identStart     = letter   <|> char '_'
  , identLetter    = alphaNum <|> char '_'
  , opStart        = undefined
  , opLetter       = undefined
  , reservedNames  =
      ["absolute", "array", "asm", "begin", "break", "case", "const"
     , "constructor", "continue", "destructor", "do", "downto", "else"
     , "end", "file", "for", "function", "goto", "if", "implementation"
     , "inherited", "inline", "interface", "label", "nil", "object"
     , "of", "on", "operator", "packed", "procedure", "program", "record"
     , "reintroduce", "repeat", "self", "set", "string", "then"
     , "to", "type", "unit", "until", "uses", "var", "while", "with"]
  , reservedOpNames =
      ["@", "+", "-", "*", "/", "div", "mod", "not", "and", "or", "xor"
     , "shl", "shr", "=", "<>", "<", ">", "<=", ">=", "in"]
  , caseSensitive   = False
}

lexer :: T.TokenParser ParserState
lexer = T.makeTokenParser hPascalDef


-- # Parsers básicos #


-- whitespace e símbolos

symbol     :: String -> HParser String
symbol     = T.symbol lexer          -- lexema, nao atomico

lexeme     :: HParser a -> HParser a
lexeme     = T.lexeme lexer              -- pra montar lexemas

whiteSpace :: HParser ()
whiteSpace = T.whiteSpace lexer      -- espaços em branco e comentarios


-- identificadores e palavras reservadas

reserved   :: String -> HParser ()
reserved   = T.reserved lexer    -- atomico, checa se n é seguido por mais letras

identifier :: HParser Identifier -- checa se nao é uma palavra reservada
--identifier = Identifier `liftM` (T.identifier lexer)  
identifier = T.identifier lexer <?> "identifier"

reservedOp :: String -> HParser ()
reservedOp = T.reservedOp lexer


-- char e strings

charLiteral   :: HParser Char
charLiteral   = T.charLiteral lexer    -- '\n'

stringLiteral :: HParser String
stringLiteral = T.stringLiteral lexer  -- "hasuhau\\ops\n\""


-- numbers

number :: HParser (Either Integer Double)
number = T.naturalOrFloat lexer


-- Agrupamento

parens    :: HParser a -> HParser a
parens    = T.parens lexer       -- ( p )

brackets  :: HParser a -> HParser a
brackets  = T.brackets lexer     -- [ p ]

semi      :: HParser String
semi      = T.semi lexer         -- ;

comma     :: HParser String
comma     = T.comma lexer        -- ,

dot       :: HParser String
dot       = T.dot lexer          -- .

colon     :: HParser String
colon     = T.colon lexer        -- :

commaSep  :: HParser a -> HParser [a]
commaSep  = T.commaSep lexer     -- [p {, p}]

semiSep   :: HParser a -> HParser [a]
semiSep   = T.semiSep lexer      -- [p {; p}]

commaSep1 :: HParser a -> HParser [a]
commaSep1 = T.commaSep1 lexer    -- p {, p}

semiSep1  :: HParser a -> HParser [a]
semiSep1 p =                     -- p ; p; ;;;; p ;;;
  do x <- p
     xs <- manysp
     return (x : xs)
  where manysp = 
          do many semi
             ((do x <- try p
                  xs <- manysp
                  return (x : xs)) <|> return [])
