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

import TypeSystem.Types
import Language.AST
import Language.Basic
import Language.Builtins
import Language.Scope (Scope, enterScope)

import Control.Monad (liftM)

import Data.Maybe (fromJust)
import Data.List (intercalate)
import Prelude hiding (lookup)
import Data.Map hiding (null, map)

import Text.ParserCombinators.Parsec
import Debug.Trace


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
data ParserState = ParserState {
   staticT :: [StaticData]
 , errors  :: [CompError]    -- ^ Lista de erros de compilacao
} deriving (Show)


-- | Um unico erro de compilacao.
-- Contem informacoes sobre onde o erro ocorreu no texto ('SourcePos') e
-- uma mensagem descritiva ('ErrorMsg').
data CompError = CompError SourcePos ErrorMsg


-- | Descricao de uma mensagem de erro de compilacao
data ErrorMsg  =
   TypeError             String  -- ^ Erro de tipo
 | UnknownIdentifier     String  -- ^ Identificador nao reconhecido
 | IdentifierAlreadyUsed String  -- ^ Declaracao dupla de identificador
 | WrongCallSignature    String  -- ^ Chamada errada de funcao
 | AmbiguousCall         String  -- ^ Chamada ambigua de funcao
 | MultipleInitialization


-- | Representacao de um erro de compilacao
instance Show CompError where
  show (CompError srcPos msg) =
    "Compilation Error at " ++ show srcPos ++ ":\n" ++ show msg


-- | Representacao da mensagem de um erro de compilacao
instance Show ErrorMsg where
  show (TypeError msg)             =
    "Type error: "              ++ msg
  show (UnknownIdentifier msg)     =
    "Not in scope: "            ++ msg
  show (IdentifierAlreadyUsed msg) =
    "Identifier already used: " ++ msg
  show (MultipleInitialization)    =
    "Invalid multiple variable initialization"
  show (WrongCallSignature msg)    =
    "No signature match for " ++ msg
  show (AmbiguousCall msg)         =
    "Multiple matches in call signature for " ++ msg


-- | Engloba um string em aspas duplas
-- quoted :: String -> String
-- quoted = (++ "\"") . ('"' :)


-- | Extrai uma representacao dos erros de compilacao
-- presentes em um ParserState, se existirem.
compErrors :: ParserState -> Maybe String
compErrors ps
  | null errL = Nothing
  | otherwise = Just (intercalate "\n\n" (map show errL))
 where errL = errors ps


-- | Parser utilizado para extrair a tabela de simbolos do estado interno
getHeadSymT  :: HParser SymbolTable
getHeadSymT  = (stSymT . head . staticT) `liftM` getState


-- | Parser utilizado para extrair a tabela de tipos do estado interno
getHeadTypeT :: HParser TypeTable
getHeadTypeT = (stTypeT . head . staticT) `liftM` getState


getStaticData :: HParser [StaticData]
getStaticData = staticT `liftM` getState


searchIdentifier :: (Ord a) =>
                   a
                -> (StaticData -> Map a b)
                -> [StaticData]
                -> Maybe b
searchIdentifier _ _ []      = Nothing
searchIdentifier k m (sd:ss) =
  if member k t then
    Just (fromJust look)
   else
     searchIdentifier k m ss
 where
  t    = m sd
  look = lookup k t

lookupVarIdent :: Identifier -> HParser (Maybe Type)
lookupVarIdent ident =
  do l <- getStaticData
     return $ searchIdentifier ident stSymT l


lookupTypeIdent :: Identifier -> HParser (Maybe Type)
lookupTypeIdent ident = 
  do l <- getStaticData
     return $ searchIdentifier ident stTypeT l


lookupProcIdent :: Identifier -> HParser (Maybe Procedure)
lookupProcIdent ident =
  do l <- getStaticData
     return $ searchIdentifier ident stProcT l


-- | Insere o par (identificador, tipo) na tabela de simbolos,
-- sem se preocupar se o simbolo ja estava presente
updateSymT :: Identifier -> Type -> HParser ()
updateSymT symId typeV = updateState $ \st -> 
   let
      staticTable = staticT st
      headScope   = head staticTable
      tailScopes  = tail staticTable
      symbolTable = stSymT headScope
      newSymTable = insert symId typeV symbolTable
    in
   st { staticT = headScope { stSymT = newSymTable } : tailScopes } 


-- | Insere o par (identificador, tipo) na tabela de tipos,
-- sem se preocupar se o tipo ja estava presente
updateTypeT :: Identifier -> Type -> HParser ()
updateTypeT typeId typeV = updateState $ \st ->
   let
      staticTable  = staticT st
      headScope    = head staticTable
      tailScopes   = tail staticTable
      typeTable    = stTypeT headScope
      newTypeTable = insert typeId typeV typeTable
    in
   st { staticT = headScope { stSymT = newTypeTable } : tailScopes }


updateProcT :: Bool -> StaticData -> RoutineDeclaration -> HParser ()
updateProcT putDef sd' (ProcedureDec ident sig block) = 
  updateState $ \st ->
    let
     Block decls stmt (_:ss) = block
     nblock        = Block decls stmt (sd' : newHeadScope : ss)
    
     staticTable   = staticT st
     headScope     = head staticTable
     tailScopes    = tail staticTable
     procTable     = stProcT headScope
     nProcInstance = [ProcInstance sig block]
     look          = lookup ident procTable
     
     procTableF    = 
      case look of
       Nothing       -> 
            insert ident (Procedure nProcInstance)
       Just (Procedure is)
         | putDef    -> 
            insert ident $ Procedure $
             init is ++ [ProcInstance sig nblock]
         | otherwise ->
            insert ident (Procedure (is ++ nProcInstance))
       Just _        -> error "Parser.State.updateProcT.case"
            
     newProcTable = procTableF procTable
     
     newHeadScope = headScope { stProcT = newProcTable }
     
    in
     st { staticT = newHeadScope : tailScopes }
    
updateProcT _ _ _ = error "Parser.State.updateProcT"


getScope :: HParser Scope
getScope = liftM (scope . head . staticT) getState


updateScope :: Identifier -> HParser ()
updateScope ident = 
  do oldScope <- getScope
     
     let newScope      = enterScope ident oldScope
         newStaticData = StaticData {
                            stSymT  = empty
                          , stTypeT = empty
                          , stProcT = empty
                          , scope   = newScope
                         }

     updateState $ \st ->
      st { staticT = newStaticData : staticT st }


withNewScope :: Identifier -> HParser a -> HParser (a, StaticData)
withNewScope newScope p =
  do oldSd <- getStaticData
    
     updateScope newScope
     result <- p
     
     newHeadSd <- liftM head getStaticData
     
     updateState $ \st -> st { staticT = oldSd }
     
     return (result, newHeadSd)


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
initialState = ParserState {
   staticT = StaticData {
                stSymT  = builtinSymT
              , stTypeT = builtinTypeT
              , stProcT = builtinProcT
              , scope   = ["Builtins"]
             } : []
 , errors = []
}


debug :: String -> HParser ()
debug msg = trace ("[HParser] " ++ msg) return ()

debugStStr :: StaticData -> String
debugStStr sd = 
  "\n\tscope     = " ++ show (scope sd) ++
  "\n\tsymTKeys  = " ++ show (keys (stSymT sd))  ++
  "\n\tprocTKeys = " ++ show (keys (stProcT sd)) ++
  "\n\ttypeTKeys = " ++ show (keys (stTypeT sd)) ++ "\n"

debugStsStr :: [StaticData] -> String
debugStsStr = concat . (map debugStStr)

debugSt :: StaticData -> HParser ()
debugSt = debug . debugStStr

debugSts :: HParser()
debugSts = liftM staticT getState >>= debugSts'

debugSts' :: [StaticData] -> HParser ()
debugSts' sds = trace "Lots: "$ mapM_ (trace "\t" debugSt) sds
