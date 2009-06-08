-- | Contem funcoes responsaveis pelas checagens de semantica
-- estatica de um programa HPascal.
--
-- Inclui checagem de tipos, definicao de tipos invalida,
-- re-definicao de identificadores, identificadores desconhecidos, etc.

module TypeChecker where

import Language
import ParsingState

import Data.Map
import Control.Monad (forM_)


-- | Parser responsavel por processar uma declaracao de
-- variavel ('VariableDeclaration').
-- 
-- Resolve o tipo de variavel para um tipo concreto e 
-- chama 'insertSymbol' para inseri-la na tabela interna
processVarDecl :: VariableDeclaration -> HParser ()
processVarDecl (VarDec idl typeD) =
  do typeV <- getType typeD
     forM_ idl $ \varId -> insertSymbol varId typeV


-- | Retorna o valor concreto de tipo ('Type') relativo
-- a um identificador de tipo ('TypeDefinition').
--
-- Faz um lookup na tabela interna. Caso o tipo ainda nao
-- tenha sido definido, retorna um UnknownType e loga o erro
-- no estado interno do parser.
getType :: TypeDefinition -> HParser Type
getType typeD =
  do tTable <- getTypeT
     case mlookup typeD tTable of
      Just t  -> return t
      Nothing -> do logError (UnknownIdentifier typeD)
                    return UnknownType


-- | Dado um par (identificador, tipo), tenta inseri-lo
-- na tabela de simbolos (usando 'updateSymT'), logando um erro
-- caso o simbolo ja esteja presente la.
insertSymbol :: Identifier -> Type -> HParser ()
insertSymbol symId typeV =
  do symTable <- getSymT
     case mlookup symId symTable of
      Nothing -> updateSymT symId typeV
      Just _  -> logError (IdentifierAlreadyUsed symId)


-- | Lookup num Map
mlookup :: (Ord k) => k -> Map k a -> Maybe a
mlookup = Data.Map.lookup
