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


setValue :: VariableReference
         -> Value
         -> ActiveScope
         -> ActiveScope
setValue varRef val as = as { valT = newValT }
 where
  baseVar = baseVarReference varRef
  oldValT = valT as
  newValT = insert baseVar newVal oldValT
  
  RecordVal oldM  = fromJust (lookup baseVar oldValT)
  newVal          = case varRef of
                      (VarRef _) -> val
                      _          -> setRec (fieldL varRef) oldM
  
  setRec [f]    m = RecordVal (insert f val m)
  setRec (f:fs) m = RecordVal (insert f (setRec fs fT) m)
    where RecordVal fT = fromJust (lookup f m)
  setRec _      _ = error "Eval.Runtime.setValue"
    
  fieldL (VarRef _)     = []
  fieldL (FieldRef v i) = fieldL v ++ [i]
  
   
getValue :: VariableReference
         -> ActiveScope
         -> Value
getValue (VarRef v)        as = fromJust (lookup v (valT as))
getValue (FieldRef vref f) as = fromJust (lookup f m)
  where RecordVal m = getValue vref as


insertVal :: (MonadState RuntimeData m) => VariableReference -> Value -> m ()
insertVal varRef val = modifyRefEnv modifyF
  where
    insertV as = ((), setValue varRef val as)
    modifyF re = case searchScopes (containsVar varRef) insertV re of
                  (Just _, re') -> re'
                  _             -> error "Eval.Runtime.insertVal"


insertRefVal :: (MonadState RuntimeData m) =>
                Reference -> VariableReference -> Value -> m ()
insertRefVal (StackReference sc _) varRef val = modifyRefEnv modifyF
 where
  insertV as = ((), setValue varRef val as)
  
  modifyF re = case searchScopes searchScope insertV re of
                (Just _, re') -> re'
                _             -> error "Eval.Runtime.insertRefVal"
 
  searchScope = (== sc) . scopeA
  

getVarValue :: (MonadState RuntimeData m) => VariableReference -> m Value
getVarValue varRef = 
  do refEnv' <- getRefEnv
     case searchScopes (containsVar varRef) f refEnv' of
      (Just v , _) -> return v
      _            -> error $ "Eval.Runtime.getVarValue: " ++ show varRef
 where
  f as = (getValue varRef as, as)

getRefVarValue :: (MonadState RuntimeData m) =>
                  Reference -> VariableReference -> m Value
getRefVarValue (StackReference sc _) varRef =
  do refEnv' <- getRefEnv
     case searchScopes searchScope f refEnv' of
      (Just v, _) -> return v
      _           -> error "Eval.Runtime.getRefVarValue"
 where
  f as        = (getValue varRef as, as)
  searchScope = (== sc) . scopeA


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
                    VariableReference
                 -> m VariableDescriptor
getVarDescriptor varRef =
  do refEnv' <- getRefEnv
     case searchScopes (containsVar varRef) f refEnv' of
      (Just vd, _) -> return vd
      _            -> error "Eval.Runtime.getVarDescriptor"
 where
  f as = (vd, as) where Just vd = lookup varId (symT as)
                        varId   = baseVarReference varRef


getVarScope :: (MonadState RuntimeData m) =>
               VariableReference
            -> m Scope
getVarScope varRef =
  do refEnv' <- getRefEnv
     case searchScopes (containsVar varRef) f refEnv' of
      (Just sc, _) -> return sc
      _            -> error "Eval.Runtime.getVarScope"
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


containsVar :: VariableReference -> ActiveScope -> Bool
containsVar varRef = (member (baseVarReference varRef)) . symT

containsProc :: Identifier -> ActiveScope -> Bool
containsProc ident = (member ident) . procT

containsFunc :: Identifier -> ActiveScope -> Bool
containsFunc ident = (member ident) . funcT


data RuntimeParameter =
   ExprParameter {
     exprParamValue :: Value
   }
 | VarParameter {
     paramId    :: VariableReference
   , paramRef   :: Maybe Reference
   , paramScope :: Scope
   , paramValue :: Value 
  }
 deriving (Show)
