-- Definicoes do tipo do Parser usado no HPascal
module ParserRun where

import qualified SymbolTable as SymT
import Text.ParserCombinators.Parsec hiding(parse)


type ParserState = (SymT.SymbolTable, SymT.TypeTable)

-- Monad de parsing, mantendo tabelas de símbolos como estado
type HParser a = GenParser Char ParserState a


parse :: HParser a            -- HPascal parser completo
      -> ParserState          -- Símbolos pré-definidos
      -> SourceName           -- Nome do arquivo do código
      -> String               -- Código
      -> Either ParseError a  -- Resultado (erro ou árvore de parsing)

parse p s src input = runParser p s src input


defaultSymbols :: ParserState
defaultSymbols = (SymT.builtinSymbols, SymT.builtinTypes)
