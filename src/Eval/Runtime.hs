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
 , funcT  :: FunctionTable
 , scopeA :: Scope
} deriving (Show)


data RuntimeData = RuntimeData {
   refEnv :: ReferenceEnv
} deriving (Show)


defaultRuntimeData :: RuntimeData
defaultRuntimeData = RuntimeData { refEnv = [builtinActiveScope] }
 where
   builtinActiveScope :: ActiveScope
   builtinActiveScope = makeActiveScope builtinStaticData
   
   builtinStaticData  :: StaticData
   builtinStaticData = head $ staticT initialState


getRefEnv :: (MonadState RuntimeData m) => m ReferenceEnv
getRefEnv = liftM refEnv get

getHeadScope :: (MonadState RuntimeData m) => m Scope
getHeadScope = liftM (scopeA . head) getRefEnv

getHeadSymT ::(MonadState RuntimeData m) => m SymbolTable
getHeadSymT = liftM (symT . head) getRefEnv


getHeadValT :: (MonadState RuntimeData m) => m ValueTable
getHeadValT = liftM (valT . head) getRefEnv

putHeadSymT :: (MonadState RuntimeData m) => SymbolTable -> m ()
putHeadSymT symT' = modify $ \st ->
  let referenceEnv = refEnv st
      headScope    = head referenceEnv
      newHeadScope = headScope { symT = symT' } in
  st { refEnv = newHeadScope : tail referenceEnv }

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



evalNewScope :: (MonadState RuntimeData m) => StaticData -> m a -> m a
evalNewScope sd eval =
 do 
    scopeNow    <- getHeadScope
    let scopeNext = scope sd
 
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
  
  newActiveScope           = makeActiveScope sd
  
  insertScope              = modifyRefEnv . (:)
  
  discardScope             = modifyRefEnv tail
    
  headScope                = liftM head getRefEnv

    
makeActiveScope :: StaticData -> ActiveScope
makeActiveScope (StaticData sdSymT sdTypeT sdProcT sdFuncT sdScope) =
  ActiveScope {
     symT   = sdSymT
   , typeT  = sdTypeT
   , valT   = empty
   , procT  = sdProcT
   , funcT  = sdFuncT
   , scopeA = sdScope
  }


insertDefVal :: (MonadState RuntimeData m) => Identifier -> m ()
insertDefVal ident =
  do symT' <- getHeadSymT
     valT' <- getHeadValT
     
     let typeV   = varType . fromJust $ lookup ident symT'
         newValT = insert ident (defVal typeV) valT'
     
     putHeadValT newValT

mergeRefTable :: (MonadState RuntimeData m) =>
                 Map Identifier Reference -> m ()
mergeRefTable t =
  do symT' <- getHeadSymT
     let rl      = toList t
         newSymT = merge rl symT'
     putHeadSymT newSymT
 where
  merge [] nSymT                = nSymT
  merge ((varId, ref):vs) nSymT = 
    let Just vd = lookup varId nSymT
        vd'     = vd { isReference = Just ref } in
    merge vs (insert varId vd' nSymT)


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


insertRefVal :: (MonadState RuntimeData m) =>
                Reference -> Value -> m ()
insertRefVal (StackReference sc varId) val = modifyRefEnv modifyF
 where
  insertV as = ((), as { valT = insert varId val (valT as) })
  
  modifyF re = case searchScopes searchScope insertV re of
                (Just _, re') -> re'
                _             -> error "Eval.Runtime.insertRefVal"
 
  searchScope = (== sc) . scopeA
  

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


getVarDescriptor :: (MonadState RuntimeData m) =>
                    Identifier
                 -> m VariableDescriptor
getVarDescriptor varId =
  do refEnv' <- getRefEnv
     case searchScopes (containsVar varId) f refEnv' of
      (Just vd, _) -> return vd
      _            -> error "Eval.Runtime.getVarDescriptor"
 where
  f as = (vd, as) where Just vd = lookup varId (symT as)


getVarScope :: (MonadState RuntimeData m) =>
               Identifier
            -> m Scope
getVarScope varId =
  do refEnv' <- getRefEnv
     case searchScopes (containsVar varId) f refEnv' of
      (Just sc, _) -> return sc
      _            -> error "Eval.Runtime.getVarDescriptor"
 where
  f as = (scopeA as, as)


getFunction :: (MonadState RuntimeData m) =>
               Identifier
            -> m Function
getFunction ident =
  do refEnv' <- getRefEnv
     case searchScopes (containsFunc ident) f refEnv' of
      (Just f', _) -> return f'
      _           -> error "Eval.Runtime.getFunction"
 where
  f as = (f', as) where Just f' = lookup ident (funcT as)


containsVar :: Identifier -> ActiveScope -> Bool
containsVar ident = (member ident) . symT

containsProc :: Identifier -> ActiveScope -> Bool
containsProc ident = (member ident) . procT

containsFunc :: Identifier -> ActiveScope -> Bool
containsFunc ident = (member ident) . funcT


data RuntimeParameter =
   ExprParameter {
     exprParamValue :: Value
   }
 | VarParameter {
     paramId    :: Identifier
   , paramRef   :: Maybe Reference
   , paramScope :: Scope
   , paramValue :: Value 
  }
 deriving (Show)
