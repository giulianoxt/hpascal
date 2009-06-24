
module Language.Tables where

import Data.Map (Map)
import Language.Scope (Scope)
import TypeSystem.Types (Type, Identifier)


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


data StaticData = StaticData {
    stSymT  :: SymbolTable
  , stTypeT :: TypeTable
  , scope   :: Scope
 } deriving (Show)
