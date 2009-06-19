-- | Contem a definicao do estado interno que o Parser
-- ira manter, durante a sua execucao, alem de funcoes
-- e parsers especificos para a sua manipulacao.
--
-- No estado interno, mantem-se um 'ParserState'. Normalmente,
-- esse estado nao e modificado, e sim simplesmente passado
-- entre todos os parsers da cadeia. Alguns parsers especiais,
-- como alguns ilustrados abaixo, irao acessar o estado e possivelmente
-- modifica-lo (utilizando o parser 'updateState' presente no Parsec)

module Parser.State where

import Language.AST
import TypeSystem.Types

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Map hiding (null, map)
import Text.ParserCombinators.Parsec


-- | Tipo de todos os parsers do HPascal.
-- Um parser de tokens de tipo Char, mantendo um estado
-- de tipo ParserState
type HParser a = GenParser Char ParserState a


-- | Estado interno do parser
--
-- As duas tabelas vao sendo montadas durante o processo de parsing.
--
-- A lista 'errors' acumula os erros de compilacao que vao sendo
-- encontrados durante o parsing, para que depois sejam mostrados
-- ao usuario. 
data ParserState = PState {
   symT   :: SymbolTable    -- ^ Tabela de simbolos
 , typeT  :: TypeTable      -- ^ Tabela de tipos
 , errors :: [CompError]    -- ^ Lista de erros de compilacao
} deriving (Show)


-- | Tabela interna de tipos. Mapeia identificadores para
-- tipos concretos.
--
-- Note que nem todos os tipos utilizados em um programa
-- estarao presentes nessa tabela, pois o HPascal permite
-- declaracao de variaveis com tipos anonimos (sem identificadores).
-- Ex: var x : array (3..5) of string;  
type TypeTable   = Map Identifier Type


-- | Tabela geral de simbolos (variaveis, subrotinas, etc.). Mapeia
-- identificadores para os tipos concretos dos simbolos.
--
-- Devido a declaracao com tipos anonimos, nem todos os tipos presentes
-- aqui estarao na tabela 'TypeTable'.
--
-- Nao guardamos o valor atual da variavel, por exemplo, pois as tabelas
-- deste modulo so serao utilizadas durante o parsing.
type SymbolTable = Map Identifier Type





-- | Um unico erro de compilacao.
-- Contem informacoes sobre onde o erro ocorreu no texto ('SourcePos') e
-- uma mensagem descritiva ('ErrorMsg').
data CompError = CompError SourcePos ErrorMsg


-- | Descricao de uma mensagem de erro de compilacao
data ErrorMsg  =
   TypeError             String  -- ^ Erro de tipo
 | UnknownIdentifier     String  -- ^ Identificador nao reconhecido
 | IdentifierAlreadyUsed String  -- ^ Declaracao dupla de identificador
 | MultipleInitialization


-- | Representacao de um erro de compilacao
instance Show CompError where
  show (CompError srcPos msg) =
    "Compilation Error at " ++ show srcPos ++ ":\n" ++ show msg


-- | Representacao da mensagem de um erro de compilacao
instance Show ErrorMsg where
  show (TypeError msg)             =
    "Type error: "              ++ quoted msg
  show (UnknownIdentifier msg)     =
    "Not in scope: "            ++ quoted msg
  show (IdentifierAlreadyUsed msg) =
    "Identifier already used: " ++ quoted msg
  show (MultipleInitialization)    =
    "Invalid multiple variable initialization"


-- | Engloba um string em aspas duplas
quoted :: String -> String
quoted = (++ "\"") . ('"' :)


-- | Extrai uma representacao dos erros de compilacao
-- presentes em um ParserState, se existirem.
compErrors :: ParserState -> Maybe String
compErrors ps
  | null errL = Nothing
  | otherwise = Just (intercalate "\n\n" (map show errL))
 where errL = errors ps


-- | Parser utilizado para extrair a tabela de simbolos do estado interno
getSymT  :: HParser SymbolTable
getSymT  = symT  `liftM` getState


-- | Parser utilizado para extrair a tabela de tipos do estado interno
getTypeT :: HParser TypeTable
getTypeT = typeT `liftM` getState


-- | Insere o par (identificador, tipo) na tabela de simbolos,
-- sem se preocupar se o simbolo ja estava presente
updateSymT :: Identifier -> Type -> HParser ()
updateSymT symId typeV = updateState $ \st -> 
  let symTable = symT st in
  st { symT = insert symId typeV symTable }


-- | Insere o par (identificador, tipo) na tabela de tipos,
-- sem se preocupar se o tipo ja estava presente
updateTypeT :: Identifier -> Type -> HParser ()
updateTypeT typeId typeV = updateState $ \st ->
  let typeTable = typeT st in
  st { typeT = insert typeId typeV typeTable }


-- | Parser utilizado para logar um erro de compilacao no
-- estado interno. O 'logError' nao interrompe o processo de
-- parsing, somente armazenando os erros encontrados ao longo do caminho.
--
-- E responsabilidade de quem chama o parser (modulo principal) de
-- extrair as mensagens de erros (utilizando 'compErrors') e exibi-las.
logError :: ErrorMsg -> HParser ()
logError msg = 
  do pos <- getPosition
     updateState $ \st ->
      let errL = errors st
          err  = CompError pos msg in
      st { errors = errL ++ [err] }


-- | Estado inicial do parser. Contem todos os nomes pre-definidos pelo
-- interpretador e uma lista vazia de erros de compilacao.
initialState   :: ParserState
initialState = PState {
   symT   = empty
 , typeT  = fromList [("integer", IntegerT), ("boolean", BooleanT)]
 , errors = []
}
