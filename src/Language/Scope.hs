
module Language.Scope where

import Language.Basic


type Scope = [Identifier]


data ScopeRelation =
   Different
 | Equal
 | Child
 | Sibling
 | Ancestor Scope
 deriving (Show)


enterScope :: Identifier -> Scope -> Scope
enterScope = (flip (++)) . (: [])


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
