-- | Contêm a definição do estado interno que o Parser
-- irá manter, durante a sua execução, além de funções
-- e parsers específicos para a sua manipulação.
--
-- No estado interno, mantêm-se um 'ParserState'. Normalmente,
-- esse estado não é modificado, e sim simplesmente passado
-- entre todos os parsers da cadeia. Alguns parsers especiais,
-- como alguns ilustrados abaixo, irão acessar o estado e possivelmente
-- modifica-lo (utilizando o parser 'updateState' presente no Parsec)

module ParsingState where

import Language

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
-- As duas tabelas vão sendo montadas durante o processo de parsing.
--
-- A lista 'errors' acumula os erros de compilação que vão sendo
-- encontrados durante o parsing, para que depois sejam mostrados
-- ao usuário. 
data ParserState = PState {
   symT   :: SymbolTable    -- ^ Tabela de símbolos
 , typeT  :: TypeTable      -- ^ Tabela de tipos
 , errors :: [CompError]    -- ^ Lista de erros de compilação
} deriving (Show)


-- | Tabela interna de tipos. Mapeia identificadores para
-- tipos concretos.
--
-- Note que nem todos os tipos utilizados em um programa
-- estarão presentes nessa tabela, pois o HPascal permite
-- declaração de variáveis com tipos anônimos (sem identificadores).
-- Ex: var x : array (3..5) of string;  
type TypeTable   = Map Identifier Type


-- | Tabela geral de símbolos (variáveis, subrotinas, etc.). Mapeia
-- identificadores para os tipos concretos dos símbolos.
--
-- Devido à declaração com tipos anônimos, nem todos os tipos presentes
-- aqui estarão na tabela 'TypeTable'.
--
-- Não guardamos o valor atual da variável, por exemplo, pois as tabelas
-- deste módulo só serão utilizadas durante o parsing.
type SymbolTable = Map Identifier Type


-- | Tipo variante em Haskell usado para representar os tipos
-- concretos presentes em programas HPascal
--
-- O tipo indefinido ('UnknownType') é utilizado quando a declaração
-- da variável contêm um tipo ilegal (ex: var x : TipoNaoDefinido;). Assim,
-- podemos continuar o processo de parsing e análise, possivelmente encontrando
-- mais erros, em vez de parar na primeira ocorrência.
data Type = 
   IntegerT     -- ^ Inteiro
 | BooleanT     -- ^ Booleano
 | UnknownType  -- ^ Tipo indefinido
 deriving (Show)


-- | Um único erro de compilação.
-- Contêm informações sobre onde o erro ocorreu no texto ('SourcePos') e
-- uma mensagem descritiva ('ErrorMsg').
data CompError = CompError SourcePos ErrorMsg


-- | Descrição de uma mensagem de erro de compilação
data ErrorMsg  =
   TypeError             String  -- ^ Erro de tipo
 | UnknownIdentifier     String  -- ^ Identificador não reconhecido
 | IdentifierAlreadyUsed String  -- ^ Declaração dupla de identificador


-- | Representação de um erro de compilação
instance Show CompError where
  show (CompError srcPos msg) =
    "Compilation Error at " ++ show srcPos ++ ":\n" ++ show msg


-- | Representação da mensagem de um erro de compilação
instance Show ErrorMsg where
  show (TypeError msg)             =
    "Type error: "              ++ quoted msg
  show (UnknownIdentifier msg)     =
    "Not in scope: "            ++ quoted msg
  show (IdentifierAlreadyUsed msg) =
    "Identifier already used: " ++ quoted msg


-- | Engloba um string em aspas duplas
quoted :: String -> String
quoted = (++ "\"") . ('"' :)


-- | Extrai uma representação dos erros de compilação
-- presentes em um ParserState, se existirem.
compErrors :: ParserState -> Maybe String
compErrors ps
  | null errL = Nothing
  | otherwise = Just (intercalate "\n\n" (map show errL))
 where errL = errors ps


-- | Parser utilizado para extrair a tabela de símbolos do estado interno
getSymT  :: HParser SymbolTable
getSymT  = symT  `liftM` getState


-- | Parser utilizado para extrair a tabela de tipos do estado interno
getTypeT :: HParser TypeTable
getTypeT = typeT `liftM` getState


-- | Insere o par (identificador, tipo) na tabela de símbolos,
-- sem se preocupar se o símbolo já estava presente
updateSymT :: Identifier -> Type -> HParser ()
updateSymT symId typeV = updateState $ \st -> 
  let symTable = symT st in
  st { symT = insert symId typeV symTable }


-- | Insere o par (identificador, tipo) na tabela de tipos,
-- sem se preocupar se o tipo já estava presente
updateTypeT :: Identifier -> Type -> HParser ()
updateTypeT typeId typeV = updateState $ \st ->
  let typeTable = typeT st in
  st { typeT = insert typeId typeV typeTable }


-- | Parser utilizado para logar um erro de compilação no
-- estado interno. O 'logError' não interrompe o processo de
-- parsing, somente armazenando os erros encontrados ao longo do caminho.
--
-- É responsabilidade de quem chama o parser (módulo principal) de
-- extrair as mensagens de erros (utilizando 'compErrors') e exibí-las.
logError :: ErrorMsg -> HParser ()
logError msg = 
  do pos <- getPosition
     updateState $ \st ->
      let errL = errors st
          err  = CompError pos msg in
      st { errors = errL ++ [err] }


-- | Estado inicial do parser. Contêm todos os nomes pré-definidos pelo
-- interpretador e uma lista vazia de erros de compilação.
initialState   :: ParserState
initialState = PState {
   symT   = empty
 , typeT  = fromList [("integer", IntegerT), ("boolean", BooleanT)]
 , errors = []
}
