{-# LANGUAGE FlexibleContexts #-}

module Eval.Runtime where

import Eval.Values
import Language.AST
import Language.Scope
import Language.Basic
import Parser.State (initialState, staticT)

import Data.Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

import Control.Monad.State



type ReferenceEnv = [ActiveScope]


data ActiveScope = ActiveScope {
   symT   :: SymbolTable
 , typeT  :: TypeTable
 , valT   :: ValueTable
 , procT  :: ProcedureTable
 , scopeA :: Scope
} deriving (Show)


data RuntimeData = RuntimeData {
   refEnv :: ReferenceEnv
} deriving (Show)


defaultRuntimeData :: RuntimeData
defaultRuntimeData = RuntimeData { refEnv = [builtinActiveScope] }
 where
   builtinActiveScope :: ActiveScope
   builtinActiveScope = makeActiveScope builtinStaticData empty
   
   builtinStaticData  :: StaticData
   builtinStaticData = head $ staticT initialState



getRefEnv :: (MonadState RuntimeData m) => m ReferenceEnv
getRefEnv = liftM refEnv get

getHeadScope :: RuntimeData -> Scope
getHeadScope = scopeA . head . refEnv

getHeadSymT ::(MonadState RuntimeData m) => m SymbolTable
getHeadSymT = liftM (symT . head) getRefEnv


getHeadValT :: (MonadState RuntimeData m) => m ValueTable
getHeadValT = liftM (valT . head) getRefEnv


putHeadValT :: (MonadState RuntimeData m) => ValueTable -> m ()
putHeadValT valueT = modify $ \st ->
 let referenceEnv = refEnv st
     headScope    = head referenceEnv
     newHeadScope = headScope { valT = valueT } in
 st { refEnv =  newHeadScope : tail referenceEnv }


putRefEnv :: (MonadState RuntimeData m) => ReferenceEnv -> m ()
putRefEnv env = modify $ \st -> st { refEnv = env }


modifyRefEnv :: (MonadState RuntimeData m) =>
                (ReferenceEnv -> ReferenceEnv) -> m()
modifyRefEnv f = getRefEnv >>= putRefEnv . f



evalNewScope :: (MonadState RuntimeData m) => StaticData -> ValueTable -> m a -> m a
evalNewScope sd valT' eval =
 do runtimeData <- get
 
    let scopeNow  = getHeadScope runtimeData
        scopeNext = scope sd
 
    case cmpScopes scopeNow scopeNext of
     Equal      -> evalEqualScope
     Child      -> evalChildScope
     Sibling    -> evalSiblingScope
     Ancestor p -> evalAncestorScope p
     _          -> error "Eval.Runtime.newScopeEval"

 where
  evalEqualScope           = evalSiblingScope
 
  evalChildScope           =
    do insertScope newActiveScope
       result <- eval
       discardScope
       return result
  
  evalSiblingScope         =
    do oldHeadScope <- headScope
       discardScope
       result       <- evalChildScope
       insertScope oldHeadScope
       return result
  
  evalAncestorScope prefix =
    do oldRefEnv <- getRefEnv
      
       modifyRefEnv dropF
       result    <- evalChildScope
       
       modifyRefEnv $ merge oldRefEnv
       return result
    where
      tailL   = length prefix - 1
      dropF l = drop (length l - tailL) l
      
      merge xs []         = xs
      merge (_:xs) (y:ys) = y : merge xs ys
      merge _ _           = error "Eval.Runtime.evalNewScope.merge"
  
  newActiveScope           = makeActiveScope sd valT'
  
  insertScope              = modifyRefEnv . (:)
  
  discardScope             = modifyRefEnv tail
    
  headScope                = liftM head getRefEnv

    
makeActiveScope :: StaticData -> ValueTable -> ActiveScope
makeActiveScope (StaticData sdSymT sdTypeT sdProcT sdScope) valT' =
  ActiveScope {
     symT   = sdSymT
   , typeT  = sdTypeT
   , valT   = valT'
   , procT  = sdProcT
   , scopeA = sdScope
  }


insertDefVal :: (MonadState RuntimeData m) => Identifier -> m ()
insertDefVal ident =
  do symT' <- getHeadSymT
     valT' <- getHeadValT
     
     let typeV   = fromJust $ lookup ident symT'
         newValT = insert ident (defVal typeV) valT'
     
     putHeadValT newValT


searchScopes :: (ActiveScope -> Bool)
             -> (ActiveScope -> (a, ActiveScope))
             -> ReferenceEnv
             -> (Maybe a, ReferenceEnv)
searchScopes _ _ []     = (Nothing, [])
searchScopes p f (s:ss)
  | p s       = let (x,s')  = f s                 in
                 (Just x, s':ss)
  | otherwise = let (x,ss') = searchScopes p f ss in
                 (x     , s:ss')


insertVal :: (MonadState RuntimeData m) => Identifier -> Value -> m ()
insertVal ident val = modifyRefEnv modifyF
  where
    insertV as = ((), as { valT = insert ident val (valT as) })
    modifyF re = case searchScopes (containsVar ident) insertV re of
                  (Just _, re') -> re'
                  _             -> error "Eval.Runtime.insertVal"


getVarValue :: (MonadState RuntimeData m) => Identifier -> m Value
getVarValue ident = 
  do refEnv' <- getRefEnv
     case searchScopes (containsVar ident) f refEnv' of
      (Just v , _) -> return v
      _            -> error $ "Eval.Runtime.getVarValue: " ++ ident
 where
  f as = (v, as) where Just v = lookup ident (valT as)


getProcedure :: (MonadState RuntimeData m) =>
                Identifier
             -> m Procedure
getProcedure ident =
  do refEnv' <- getRefEnv
     case searchScopes (containsProc ident) f refEnv' of
      (Just p, _) -> return p
      _           -> error "Eval.Runtime.getProcedure"
 where
  f as = (p, as) where Just p = lookup ident (procT as)


containsVar :: Identifier -> ActiveScope -> Bool
containsVar ident = (member ident) . symT

containsProc :: Identifier -> ActiveScope -> Bool
containsProc ident = (member ident) . procT
