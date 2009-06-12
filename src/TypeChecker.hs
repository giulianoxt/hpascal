-- | Contem funcoes responsaveis pelas checagens de semantica
-- estatica de um programa HPascal.
--
-- Inclui checagem de tipos, definicao de tipos invalida,
-- re-definicao de identificadores, identificadores desconhecidos, etc.

module TypeChecker where

import Types
import Language
import ParsingState

import Control.Monad (forM_)
import qualified Data.Map as M


-- | Lookup num Map
mlookup :: (Ord k) => k -> M.Map k a -> Maybe a
mlookup = M.lookup


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


-- | Parser responsavel por processar uma atribuicao.
--
-- Infere o tipo da expressao do lado direito e da variavel
-- do lado esquerdo, para checar compatibilidade de atribuicao.
processAssignment :: Statement -> HParser ()
processAssignment (Assignment varRef op e) =
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
     case mlookup varId sTable of
      Just t  -> return t
      Nothing -> do logError (UnknownIdentifier varId)
                    return UnknownType


-- | Funcao que implementa o mecanismo de inferencia de tipos
-- para expressoes em geral.
--
-- Utiliza casamento de padrao sobre a arvore de expressoes e,
-- para deduzir as coercoes, utiliza as funcoes exportadas pelo
-- modulo 'Types'. 
infer :: Expr -> HParser Type
infer e =
 case e of
  ConstNum  _ -> return IntegerT
  ConstBool _ -> return BooleanT
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
 
 where
  -- As funcoes abaixo encapsulam os metodos de coercao
  -- exportados pelo modulo 'Types', adicionando:
  --  - Propagacao de tipos indefinidos pela recursao
  --  - Log de erros na checagem de tipos
 
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
