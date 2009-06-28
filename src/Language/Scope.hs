-- | Modulo importante para permitir o tratamento
-- correto dos escopos criados de acordo com o codigo.
-- O HPascal trabalha apenas com escopo estatico.
module Language.Scope where

import Language.Basic

-- | Escopo. Eh representado como a lista dos
-- identificadores acessiveis dentro dele.
type Scope = [Identifier]

-- | Definicao que abstrai a relacao entre dois escopos,
-- util, por exemplo, a determinacao das permissoes de
-- acesso a variaveis.
data ScopeRelation =
   Different
 | Equal
 | Child
 | Sibling
 | Ancestor Scope
 deriving (Show)

-- | Funcao que adiciona um identificador a um determinado
-- escopo, devolvendo o escopo resultante.
enterScope :: Identifier -> Scope -> Scope
enterScope = (flip (++)) . (: [])

-- | Funcao de comparacao entre escopos. Recebe duas
-- entidades representativas de escopos e devolve a
-- relacao existente entre elas.
cmpScopes :: Scope -> Scope -> ScopeRelation
cmpScopes s1 s2
  | s1 == s2          = Equal
  | null prefix       = Different
  | prefix == s1      = Child
  | prefix == init s1 = Sibling
  | prefix == s2      = Ancestor prefix
  | otherwise         = error "Language.Scope.cmpScopes"
 where
  prefix :: Scope
  prefix = lprefix s1 s2

  lprefix :: (Eq a) => [a] -> [a] -> [a]
  lprefix (x:xs) (y:ys)
    | x == y    = x : lprefix xs ys
    | otherwise = []
  lprefix _ _           = []
