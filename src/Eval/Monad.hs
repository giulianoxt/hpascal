
module Eval.Monad where

import Language.AST
import Eval.Values
import Eval.Runtime
import TypeSystem.Types (Identifier)

import Control.Monad.State


type HEval a = StateT RuntimeEnv IO a


eval :: Program -> IO ()
eval p = execStateT (evalProgram p) defaultRuntimeEnv 
      >> return ()


evalProgram :: Program -> HEval ()
evalProgram (Program _ _ block) = evalBlock block


-- | TODO: pensar em escopos, recursao, etc.
evalBlock :: Block -> HEval ()
evalBlock (Block decls stmt sd) =
  do modify $ updateRuntimeEnv sd
     evalDeclarations decls
     evalStatement stmt


evalDeclarations :: DeclarationPart -> HEval ()
evalDeclarations (DeclPart _ _ varL _) =
  forM_ varL evalVarDeclaration


evalVarDeclaration :: VariableDeclaration -> HEval ()
evalVarDeclaration (VarDec idl _ Nothing) =
  forM_ idl putDefVal
  
  where putDefVal :: Identifier -> HEval ()
        putDefVal ident = modify (insertVarDefVal ident)
  
evalVarDeclaration _ = error "TODO: evalVarDeclaration"



evalStatement :: Statement -> HEval ()

evalStatement (Compound stmtl) = forM_ stmtl evalStatement

evalStatement (Assignment ":=" ident expr) =
  do exprV <- evalExpr expr
     modify (updateValTable ident exprV)  

evalStatement (ProcedureCall "writeln" [e]) =
  do val <- evalExpr e
     liftIO $ putStrLn (show val)

evalStatement (If e st1 st2) =
  do BoolVal b <- evalExpr e
     if (b) then
       evalStatement st1
      else
       evalStatement st2

evalStatement while@(While e stmt) =
  do BoolVal b <- evalExpr e
     when b $ evalStatement stmt
           >> evalStatement while

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
    e1 `In` e2 -> error "Monad.evalExpr"
    
    e1 :+: e2 -> bin e1 e2 $ numOp2 (+)
    e1 :-: e2 -> bin e1 e2 $ numOp2 (-)
    e1 :*: e2 -> bin e1 e2 $ numOp2 (*)
    e1 :/: e2 -> bin e1 e2 $ numOp2 (div)
    
    Not e     -> unary e   $ boolOp1 not
    Var varR  -> evalVarReference varR
    
    ConstExpr c -> evalConstant c
    
 where bin e1 e2 f = 
        do v1 <- evalExpr e1
           v2 <- evalExpr e2
           return $ f v1 v2
       
       unary e f = liftM f (evalExpr e)


evalVarReference :: VariableReference -> HEval Value
evalVarReference ident =
  do runtimeEnv <- get
     return $ varValue ident runtimeEnv


evalConstant :: Constant -> HEval Value
evalConstant c =
  case c of
    ConstNum  n -> return (IntVal n)
    ConstBool b -> return (BoolVal b)
    ConstStr  s -> return (StringVal s)
    _           -> error "Monad.evalConstant"
