-- | Definicoes necessarias para a execucao do parser principal
module ParserRun where

import Language
import Parsing (hparser)
import ParsingState (initialState, ParserState)
import Text.ParserCombinators.Parsec hiding(parse)


-- | Funcao para execucao do parser principal do HPascal
parse :: SourceName                                -- ^ Nome do arquivo do codigo
      -> String                                    -- ^ Codigo
      -> Either ParseError (Program, ParserState)  -- ^ Erro ou arvore de parsing e estado final
          
parse src input = runParser hparser initialState src input
