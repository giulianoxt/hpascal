-- | Definições necessárias para a execução do parser principal
module ParserRun where

import Language
import Parsing (hparser)
import ParsingState (initialState, ParserState)
import Text.ParserCombinators.Parsec hiding(parse)


-- | Função para execução do parser principal do HPascal
parse :: SourceName     -- ^ Nome do arquivo do código
      -> String         -- ^ Código
      -> Either         -- ^ Erro ou árvore de parsing e estado final
          ParseError (Program, ParserState)  
parse src input = runParser hparser initialState src input
