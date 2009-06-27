{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- | Contem funcoes responsaveis pelas checagens de semantica
-- estatica de um programa HPascal.
--
-- Inclui checagem de tipos, definicao de tipos invalida,
-- re-definicao de identificadores, identificadores desconhecidos, etc.

module TypeSystem.Checker where

import Language.AST
import Language.Basic
import Parser.State
import TypeSystem.Types

import Data.Map (lookup)
import Prelude hiding (lookup)

import Control.Monad (when, forM_)


-- | Parser responsavel por processar uma declaracao de
-- variavel ('VariableDeclaration').
-- 
-- Resolve o tipo de variavel para um tipo concreto e 
-- chama 'insertSymbol' para inseri-la na tabela interna
processVarDecl :: VariableDeclaration -> HParser ()
processVarDecl (VarDec (_:_:_) _ (Just _))=
  logError MultipleInitialization
processVarDecl (VarDec idl typeV mexpr) =
  forM_ idl $ \varId ->
    do insertVar varId typeV False Nothing
       case mexpr of
        Just e  -> processAssignment (Assignment varId e) >> return ()
        Nothing -> return ()


processConstDecl :: ConstantDeclaration -> HParser ()
processConstDecl (ConstDec varId mtype expr) =
  do typeV <- case mtype of
                Nothing -> infer expr
                Just t  -> return t
     
     insertVar varId typeV True Nothing
     checkCompatibleExprs (Var varId) expr


processParams :: [Parameter] -> HParser ()
processParams = mapM_ singleParam
 where
  singleParam (Parameter _ (_:_:_) _ (Just _)) =
    logError MultipleInitialization
  
  singleParam (Parameter Reference [v] _ (Just _)) =
    logError $ ReferenceAssignment $ show v
  
  singleParam (Parameter mode idl typeV mexpr) =
    forM_ idl $ \varId ->
      do let insertF = insertVar varId typeV 
       
         case mode of
          Value     -> insertF False Nothing
          Const     -> insertF True  Nothing
          Reference -> insertF False $ Just (error "singleParam")
         
         case mexpr of
          Just e    -> do processAssignment (Assignment varId e)
                          return ()
          _         -> return ()
  
  singleParam _ = error "TypeSystem.Checker.processParams"


-- | Dado um par (identificador, tipo), tenta inseri-lo
-- na tabela de simbolos (usando 'updateSymT'), logando um erro
-- caso o simbolo ja esteja presente la.
insertVar :: Identifier -> Type -> Bool -> (Maybe Reference) -> HParser ()
insertVar varId typeV isConst' ref =
  do symTable <- getHeadSymT
     case lookup varId symTable of
      Nothing -> updateSymT varId varD
      Just _  -> logError (IdentifierAlreadyUsed varId)
 where
  varD = VarDescriptor typeV isConst' ref


processProcCall :: Statement -> HParser Statement
processProcCall call@(ProcedureCall ident exprs _) =
  do look <- getProcVal ident
     case look of
       Nothing                     -> return Nop
       Just (Procedure sigs)       -> checkCall sigs
       Just (HaskellProc checkF _) -> checkHCall checkF
 where
  checkCall sigs = 
    do types <- mapM infer exprs
       
       let sigs' = matchProcCall types (map psignature sigs)
       
       case length sigs' of
        0 -> do logError $ WrongCallSignature $
                    "procedure "
                 ++ show ident
                 ++ ", called with: "
                 ++ show types
                return call
              
        1 -> return (ProcedureCall ident exprs $ (snd . head) sigs')
        
        _ -> do logError $ AmbiguousCall $
                    "procedure "
                 ++ show ident
                 ++ ". Options: "
                 ++ show sigs'
                return call

  checkHCall checkF =
    do types <- mapM infer exprs
       
       when (not $ checkF types) $
         do logError $ WrongCallSignature $
                "procedure "
              ++ show ident
              ++ ", called with: "
              ++ show types                        
       
       return call

processProcCall _ = error "TypeSystem.Checker.processProcCall"



processFuncCall :: Expr -> HParser Expr
processFuncCall call@(FunctionCall ident exprs _) =
  do look <- getFuncVal ident
     case look of
      Nothing                       -> return call
      Just (Function sigs)          -> checkCall sigs
      Just (HaskellFunc checkF _ _) -> checkHCall checkF
 where
  checkCall sigs =
    do types <- mapM infer exprs
      
       let sigs' = matchProcCall types (map fsignature sigs)
       
       case length sigs' of
        0 -> do logError $ WrongCallSignature $
                    "function "
                 ++ show ident
                 ++ ", called with: "
                 ++ show types
                return call

        1 -> return (FunctionCall ident exprs $ (snd . head) sigs')
        
        _ -> do logError $ AmbiguousCall $
                    "function "
                 ++ show ident
                 ++ ". Options: "
                 ++ show sigs'
                return call

  checkHCall checkF =
    do types <- mapM infer exprs
      
       when (not $ checkF types) $
         do logError $ WrongCallSignature $
                "procedure "
              ++ show ident
              ++ ", called with: "
              ++ show types    
       
       return call

processFuncCall _ = error "TypeSystem.Checker.processFuncCall"


-- | Parser responsavel por processar uma atribuicao.
--
-- Infere o tipo da expressao do lado direito e da variavel
-- do lado esquerdo, para checar compatibilidade de atribuicao.
processAssignment :: Statement -> HParser Statement
processAssignment assign@(Assignment varRef e) =
  do onF <- isOnFunction
     
     let check'  = checkAssignExpr varRef e >> return assign
         fReturn = FunctionReturn varRef e 
     
     case onF of
      Nothing -> check'
      Just (fId, t1)
        | varRef /= fId -> check'
        | otherwise     -> do
            t2 <- infer e
            if (t1 /= UnknownType && t2 /= UnknownType) then
              case cAssign t1 t2 of
                Right _  -> return fReturn
                Left msg -> logError (TypeError msg) >> return Nop
             else
               return Nop
      
processAssignment _ = error "TypeSystem.Checker.processAssignment"


checkBooleanExpr :: Expr -> HParser ()
checkBooleanExpr = checkExprType BooleanT

checkOrdinalExpr :: Expr -> HParser ()
checkOrdinalExpr = checkExprType IntegerT

checkAssignExpr :: VariableReference -> Expr -> HParser ()
checkAssignExpr varId e =
  do checkCompatibleExprs (Var varId) e
     b <- isConstVar varId
     when b $ logError (ConstAssignment varId)

checkCaseMatch :: Expr -> CaseMatch -> HParser ()
checkCaseMatch e m =
  case m of
    SingleCase c    -> compatible c
    RangeCase c1 c2 -> compatible c1 >> compatible c2
 where
  compatible = (checkCompatibleExprs e) . ConstExpr

checkCompatibleExprs :: Expr -> Expr -> HParser ()
checkCompatibleExprs e1 e2 =
  do t1 <- infer e1
     t2 <- infer e2
     when (t1 /= UnknownType && t2 /= UnknownType) $
      case cAssign t1 t2 of
        Right _  -> return ()
        Left msg -> logError (TypeError msg)


checkExprType :: Type -> Expr -> HParser ()
checkExprType typeV expr =
  do eT <- infer expr
     check' eT typeV
 where check' e1 e2
        | elem e1 [UnknownType,e2] = return ()
        | otherwise = logError $ TypeError $
            "Expecting " ++ show typeV ++ " but inferred "
                         ++ show e2    ++ " for expression"
       
       
getVarType :: VariableReference -> HParser Type
getVarType varId = 
  do look <- lookupVarIdent varId
     case look of
      Just t  -> return t
      Nothing -> do logError $ UnknownIdentifier $
                      "var identifier " ++ show varId
                    return UnknownType


getTypeVal :: Identifier -> HParser Type
getTypeVal typeId =
  do look <- lookupTypeIdent typeId
     case look of
      Just t  -> return t
      Nothing -> do logError $ UnknownIdentifier $
                     "type identifier " ++ show typeId
                    return UnknownType


getProcVal :: Identifier -> HParser (Maybe Procedure)
getProcVal procId =
  do look <- lookupProcIdent procId
     case look of
      Just _  -> return ()
      Nothing -> logError $ UnknownIdentifier $
                  "procedure " ++ show procId
     return look


getFuncVal :: Identifier -> HParser (Maybe Function)
getFuncVal funcId =
  do look <- lookupFuncIdent funcId
     case look of
      Just _  -> return ()
      Nothing -> logError $ UnknownIdentifier $
                  "function " ++ show funcId
     return look


-- | Funcao que implementa o mecanismo de inferencia de tipos
-- para expressoes em geral.
--
-- Utiliza casamento de padrao sobre a arvore de expressoes e,
-- para deduzir as coercoes, utiliza as funcoes exportadas pelo
-- modulo 'Types'. 
infer :: Expr -> HParser Type
infer e =
 case e of
  ConstExpr c -> constInf c
  Var varId   -> getVarType varId
  
  Minus e1    -> unaryInf cNumOp1  e1
  Not e1      -> unaryInf cBoolOp1 e1
  
  e1 :*: e2   -> binaryInf cNumOp2 e1 e2
  e1 :-: e2   -> binaryInf cNumOp2 e1 e2
  e1 :+: e2   -> binaryInf cNumOp2 e1 e2
  e1 :/: e2   -> binaryInf cDivOp2 e1 e2
  
  e1 :<>: e2  -> binaryInf cEqOp2 e1 e2
  e1 :>=: e2  -> binaryInf cRelOp2 e1 e2
  e1 :<=: e2  -> binaryInf cRelOp2 e1 e2
  e1 :=: e2   -> binaryInf cEqOp2 e1 e2
  e1 :>: e2   -> binaryInf cRelOp2 e1 e2
  e1 :<: e2   -> binaryInf cRelOp2 e1 e2
  _ `In` _    -> error "TypeChecker.infer"
  
  e1 :**: e2  -> binaryInf cNumOp2 e1 e2
  e1 `Shr` e2 -> binaryInf cNumOp2 e1 e2
  e1 `Shl` e2 -> binaryInf cNumOp2 e1 e2
  e1 `And` e2 -> binaryInf cBoolOp2 e1 e2
  e1 `Xor` e2 -> binaryInf cBoolOp2 e1 e2
  e1 `Or` e2  -> binaryInf cBoolOp2 e1 e2
  e1 `Mod` e2 -> binaryInf cNumOp2 e1 e2
  e1 `Div` e2 -> binaryInf cNumOp2 e1 e2
  
  FunctionCall fId _ sigP ->
    do look <- getFuncVal fId
       case look of
        Nothing                  -> error "TypeChecker.infer.FunctionCall"
        Just (Function sigs)     -> return $ fInstanceType (sigs !! sigP)
        Just (HaskellFunc _ t _) -> return t

-- As funcoes abaixo encapsulam os metodos de coercao
-- exportados pelo modulo 'Types', adicionando:
--  - Propagacao de tipos indefinidos pela recursao
--  - Log de erros na checagem de tipos
 
constInf :: Constant -> HParser Type
constInf (ConstBool _) = return BooleanT
constInf (ConstStr  _) = return StringT
constInf (ConstNum (FloatNum _)) = return FloatT
constInf (ConstNum (IntNum _))   = return IntegerT
constInf _             = error "TypeChecker.constInf"

unaryInf  :: UnaryCoercion
          -> Expr
          -> HParser Type
unaryInf coerce e' = do
  t <- infer e'
  if t == UnknownType then
     return UnknownType
    else
     returnCoerce (coerce t)

binaryInf :: BinaryCoercion
          -> Expr
          -> Expr
          -> HParser Type
binaryInf coerce e1 e2 = do
  t1 <- infer e1
  t2 <- infer e2
  if (t1 == UnknownType || t2 == UnknownType) then
     return UnknownType
    else
     returnCoerce (coerce t1 t2)

returnCoerce :: CoerceResult -> HParser Type
returnCoerce cr = case cr of
  Right t  -> return t
  Left msg -> do logError $ TypeError msg
                 return UnknownType
