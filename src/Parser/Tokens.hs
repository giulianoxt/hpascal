-- | Definicoes dos parsers para os 
-- diversos lexemas do HPascal
-- usa as facilidades providas pelo Parsec.Token
module Parser.Tokens where

import Parser.State (HParser, ParserState)

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
     , "inherited", "inline", "interface", "label", "nil", "object", "false"
     , "of", "on", "operator", "packed", "procedure", "program", "record"
     , "reintroduce", "repeat", "self", "set", "string", "then", "true"
     , "to", "type", "unit", "until", "uses", "var", "while", "with"]
  , reservedOpNames =
      ["@", "+", "-", "*", "/", "div", "mod", "not", "and", "or", "xor"
     , "shl", "shr", "=", "<>", "<", ">", "<=", ">=", "in"]
  , caseSensitive   = False
}

lexer :: T.TokenParser ParserState
lexer = T.makeTokenParser hPascalDef


-- * Parsers basicos


-- ** Whitespace e simbolos


-- | Lexema, nao atomico
symbol     :: String -> HParser String
symbol     = T.symbol lexer

-- | Para montar lexemas 
lexeme     :: HParser a -> HParser a
lexeme     = T.lexeme lexer

-- | Espacos em  branco e comentarios
whiteSpace :: HParser ()
whiteSpace = T.whiteSpace lexer


-- ** Identificadores e palavras reservadas

-- | Atomico, checa se nao e seguido por mais letras
reserved   :: String -> HParser ()
reserved   = T.reserved lexer

-- | Identificador, checa se nao e uma palavra reservada
identifier :: HParser String 
identifier = T.identifier lexer <?> "identifier"

-- | Operador reservado
reservedOp :: String -> HParser ()
reservedOp = T.reservedOp lexer


-- ** Char, strings e numeros

-- | @'\n'@
charLiteral   :: HParser Char
charLiteral   = T.charLiteral lexer

-- | @"\hasuhau\\ops\n\""@
stringLiteral :: HParser String
stringLiteral = T.stringLiteral lexer


-- | Inteiros ou floats, sem sinal
number :: HParser (Either Integer Double)
number = T.naturalOrFloat lexer


-- * Parsers de agrupamento

-- | ( p )
parens    :: HParser a -> HParser a
parens    = T.parens lexer

-- | [ p ]
brackets  :: HParser a -> HParser a
brackets  = T.brackets lexer

-- | ;
semi      :: HParser String
semi      = T.semi lexer

-- | ,
comma     :: HParser String
comma     = T.comma lexer

-- | .
dot       :: HParser String
dot       = T.dot lexer

-- | :
colon     :: HParser String
colon     = T.colon lexer

-- | [p {, p}]
commaSep  :: HParser a -> HParser [a]
commaSep  = T.commaSep lexer

-- | p {, p}
commaSep1 :: HParser a -> HParser [a]
commaSep1 = T.commaSep1 lexer    

-- | p ; p; ; ;; ; p ;;; p [;;;]
semiSep1  :: HParser a -> HParser [a]
semiSep1 p =                     
  do x <- p
     xs <- manysp
     return (x : xs)
  where manysp =
         (try $ do many1 semi
                   x <- p
                   xs <- manysp
                   return (x : xs))
         <|>
         (many semi >> return [])

semiSep :: HParser a -> HParser [a]
semiSep p = option [] (semiSep1 p)
