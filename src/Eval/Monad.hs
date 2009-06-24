
module Eval.Monad where

import Language.AST
import Eval.Values
import Eval.Runtime

import Control.Monad.State


type HEval a = StateT RuntimeData IO a


eval :: Program -> IO ()
eval p = evalStateT (evalProgram p) defaultRuntimeData


evalProgram :: Program -> HEval ()
evalProgram (Program _ _ block) = evalBlock block


evalBlock :: Block -> HEval ()
evalBlock (Block decls stmt (sd:_)) =
  evalNewScope sd $ evalDeclarations decls >>
                    evalStatement stmt
evalBlock _ = error "Eval.Monad.evalBlock"


evalDeclarations :: DeclarationPart -> HEval ()
evalDeclarations (DeclPart _ _ varL _) =
  mapM_ evalVarDeclaration varL


evalVarDeclaration :: VariableDeclaration -> HEval ()

evalVarDeclaration (VarDec idl _ Nothing)         =
  mapM_ insertDefVal idl

evalVarDeclaration (VarDec [ident] _ (Just expr)) =
  evalStatement (Assignment ident expr)
  
evalVarDeclaration _                              =
  error "Eval.Monad.evalVarDeclaration"



evalStatement :: Statement -> HEval ()

evalStatement (Compound stmtl) =
  mapM_ evalStatement stmtl

evalStatement (Assignment ident expr) =
  do value <- evalExpr expr
     insertVal ident value

evalStatement (ProcedureCall "writeln" [e]) =
  do val <- evalExpr e
     liftIO $ putStrLn (show val)

evalStatement (If e st1 st2) =
  do BoolVal b <- evalExpr e
     if b then
       evalStatement st1
      else
       evalStatement st2

evalStatement repeatSt@(Repeat stmt e) =
  do evalStatement stmt
     BoolVal b <- evalExpr e
     unless b $ evalStatement repeatSt

evalStatement while@(While e stmt) =
  do BoolVal b <- evalExpr e
     when b $ evalStatement stmt >>
              evalStatement while

evalStatement _ = error "Monad.evalStatement"



evalExpr :: Expr -> HEval Value
evalExpr expr =
  case expr of
    e1 :=: e2  -> bin e1 e2 eqOp2
    e1 :<: e2  -> bin e1 e2 $ relOp2 (<)
    e1 :>: e2  -> bin e1 e2 $ relOp2 (>)
    e1 :<=: e2 -> bin e1 e2 $ relOp2 (<=)
    e1 :>=: e2 -> bin e1 e2 $ relOp2 (>=)
    e1 :<>: e2 -> bin e1 e2 $ relOp2 (/=)
    
    e1 :+: e2 -> bin e1 e2 $ numOp2 (+)
    e1 :-: e2 -> bin e1 e2 $ numOp2 (-)
    e1 :*: e2 -> bin e1 e2 $ numOp2 (*)
    e1 :/: e2 -> bin e1 e2 $ numOp2 (div)
    
    Not e     -> unary e   $ boolOp1 not
    Var varR  -> evalVarReference varR
    
    ConstExpr c -> evalConstant c
    
    _         -> error "Eval.Monad.evalExpr"
    
 where bin e1 e2 f = 
        do v1 <- evalExpr e1
           v2 <- evalExpr e2
           return $ f v1 v2
       
       unary e f = liftM f (evalExpr e)


evalVarReference :: VariableReference -> HEval Value
evalVarReference ident = getVarValue ident


evalConstant :: Constant -> HEval Value
evalConstant c =
  case c of
    ConstNum  n -> return (IntVal n)
    ConstBool b -> return (BoolVal b)
    ConstStr  s -> return (StringVal s)
    _           -> error "Monad.evalConstant"
