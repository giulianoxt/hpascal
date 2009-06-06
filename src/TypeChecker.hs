-- Modulo contendo funçoes que modificam
-- o estado interno da monad do parser, montando
-- a tabela de simbolos e de tipos

module TypeChecker where

import Language
import SymbolTable
import ParserRun (HParser)

import Control.Monad (forM_, liftM)
import Data.Map (insert, lookup, Map)
import Text.ParserCombinators.Parsec (getState, updateState)


-- Retorna a tabela de simbolos no estado do parser
symT :: HParser SymbolTable
symT  = liftM fst getState


-- Retorna a tabela de tipos no estado do parser
typeT :: HParser TypeTable
typeT = liftM snd getState


-- Atualiza a tabela de simbolos interna atraves de f
updateSymT :: (SymbolTable -> SymbolTable) -> HParser ()
updateSymT f = updateState (\(st,tt) -> (f st, tt))


-- Atualiza a tabela de tipos interna atraves de f
updateTypeT :: (TypeTable -> TypeTable) -> HParser ()
updateTypeT f = updateState (\(st,tt) -> (st, f tt))


-- Processa um bloco de declaracoes (ja parseado)
-- atualizando as tabelas internas
processDeclPart :: DeclarationPart -> HParser()
processDeclPart (DeclPart vds) = forM_ vds processVarDecl


-- Processa uma declaracao de variavel
processVarDecl :: VariableDeclaration -> HParser ()
processVarDecl (VarDec idl typeD) =
  forM_ idl $ \varId ->
    do typeV <- getType typeD
       updateSymT $ insert varId (defaultV typeV, typeD)


-- Retorna o valor concreto de tipo (Type) relativo
-- a um identificador de tipo (TypeDefinition)
-- faz um lookup na tabela interna
getType :: TypeDefinition -> HParser Type
getType typeD = do
  tTable <- typeT
  case mlookup typeD tTable of
    Just t  -> return t
    Nothing -> fail $ "Unknown type: " ++ typeD


-- Lookup num Map
mlookup :: (Ord k) => k -> Map k a -> Maybe a
mlookup = Data.Map.lookup

