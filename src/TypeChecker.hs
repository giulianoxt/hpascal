-- | Contem funcoes responsaveis pelas checagens de semantica
-- estatica de um programa HPascal.
--
-- Inclui checagem de tipos, definicao de tipos invalida,
-- re-definicao de identificadores, identificadores desconhecidos, etc.

module TypeChecker where

import Types
import Language
import ParsingState

import Data.Map (lookup)
import Prelude hiding (lookup)

import Control.Monad (forM_)


-- | Parser responsavel por processar uma declaracao de
-- variavel ('VariableDeclaration').
-- 
-- Resolve o tipo de variavel para um tipo concreto e 
-- chama 'insertSymbol' para inseri-la na tabela interna
processVarDecl :: VariableDeclaration -> HParser ()
processVarDecl (VarDec (_:_:_) _ (Just _)) =
  logError MultipleInitialization
processVarDecl (VarDec idl typeV mexpr) =
  forM_ idl $ \varId ->
    do insertSymbol varId typeV
       case mexpr of
        Just e  -> processAssignment (Assignment ":=" varId e)
        Nothing -> return ()


-- | Dado um par (identificador, tipo), tenta inseri-lo
-- na tabela de simbolos (usando 'updateSymT'), logando um erro
-- caso o simbolo ja esteja presente la.
insertSymbol :: Identifier -> Type -> HParser ()
insertSymbol symId typeV =
  do symTable <- getSymT
     case lookup symId symTable of
      Nothing -> updateSymT symId typeV
      Just _  -> logError (IdentifierAlreadyUsed symId)


-- | Parser responsavel por processar uma atribuicao.
--
-- Infere o tipo da expressao do lado direito e da variavel
-- do lado esquerdo, para checar compatibilidade de atribuicao.
processAssignment :: Statement -> HParser ()
processAssignment (Assignment op varRef e) =
  do te <- infer e
     tv <- getVarType varRef
     
     if (te == UnknownType || tv == UnknownType) then
       return ()
      else case cAssign op tv te of
            Right _  -> return ()
            Left msg -> logError (TypeError msg)
processAssignment _ = error "TypeChecker.processAssignment"


-- | Dada uma referencia para variavel, retorna o tipo dela.
-- Loga um erro caso a variavel nao seja encontrada.
getVarType :: VariableReference -> HParser Type
getVarType varId = 
  do sTable <- getSymT
     case lookup varId sTable of
      Just t  -> return t
      Nothing -> do logError (UnknownIdentifier varId)
                    return UnknownType


getTypeVal :: Identifier -> HParser Type
getTypeVal typeId =
  do tTable <- getTypeT
     case lookup typeId tTable of
      Just t  -> return t
      Nothing -> do logError (UnknownIdentifier typeId)
                    return UnknownType


processBooleanExpr :: Expr -> HParser ()
processBooleanExpr = checkExprType BooleanT

checkExprType :: Type -> Expr -> HParser ()
checkExprType typeV expr =
  do eT <- infer expr
     check eT typeV
 where check e1 e2
        | elem e1 [UnknownType,e2] = return ()
        | otherwise = logError $ TypeError $
            "Expecting " ++ show typeV ++ " but inferred "
                         ++ show e2    ++ " for expression"       


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
  e1 :/: e2   -> binaryInf cNumOp2 e1 e2
  
  e1 :<>: e2  -> binaryInf cEqOp2 e1 e2
  e1 :>=: e2  -> binaryInf cRelOp2 e1 e2
  e1 :<=: e2  -> binaryInf cRelOp2 e1 e2
  e1 :=: e2   -> binaryInf cEqOp2 e1 e2
  e1 :>: e2   -> binaryInf cRelOp2 e1 e2
  e1 :<: e2   -> binaryInf cRelOp2 e1 e2
  _ `In` _    -> undefined
  
  e1 :**: e2  -> binaryInf cNumOp2 e1 e2
  e1 `Shr` e2 -> binaryInf cNumOp2 e1 e2
  e1 `Shl` e2 -> binaryInf cNumOp2 e1 e2
  e1 `And` e2 -> binaryInf cBoolOp2 e1 e2
  e1 `Xor` e2 -> binaryInf cBoolOp2 e1 e2
  e1 `Or` e2  -> binaryInf cBoolOp2 e1 e2
  e1 `Mod` e2 -> binaryInf cNumOp2 e1 e2
  e1 `Div` e2 -> binaryInf cNumOp2 e1 e2
 
-- As funcoes abaixo encapsulam os metodos de coercao
-- exportados pelo modulo 'Types', adicionando:
--  - Propagacao de tipos indefinidos pela recursao
--  - Log de erros na checagem de tipos
 
constInf :: Constant -> HParser Type
constInf (ConstNum  _) = return IntegerT
constInf (ConstBool _) = return BooleanT
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
