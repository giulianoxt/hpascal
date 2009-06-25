
module Eval.Monad where

import Eval.Values
import Eval.Runtime
import Language.AST
import Language.Basic

import Control.Monad.State
import Data.Map hiding (map)


type HEval a = StateT RuntimeData IO a


eval :: Program -> IO ()
eval p = evalStateT (evalProgram p) defaultRuntimeData


evalProgram :: Program -> HEval ()
evalProgram (Program _ _ block) = evalBlock block


evalBlock :: Block -> HEval ()
evalBlock (Block decls stmt (sd:_)) =
  evalNewScope sd $ do evalDeclarations decls
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

evalStatement (Nop) = return ()

evalStatement (Compound stmtl) =
  mapM_ evalStatement stmtl

evalStatement (Assignment ident expr) =
  do value <- evalExpr expr
     insertVal ident value

evalStatement (FunctionReturn fId expr) =
  evalStatement (Assignment fId expr)
 
evalStatement (ProcedureCall ident exprs sigPos) =
  do proc    <- getProcedure ident
     params  <- mapM evalExpr exprs
     case proc of
      (Procedure sigs)  -> evalPascalProc (sigs !! sigPos) params
      (HaskellProc _ _) -> evalHaskellProc proc params

evalStatement (If e st1 st2) =
  do BoolVal b <- evalExpr e
     if b then
       evalStatement st1
      else
       evalStatement st2

evalStatement repeat'@(Repeat stmt e) =
  do evalStatement stmt
     BoolVal b <- evalExpr e
     unless b $ evalStatement repeat'

evalStatement while@(While e stmt) =
  do BoolVal b <- evalExpr e
     when b $ evalStatement stmt >>
              evalStatement while

evalStatement s = error $ "Monad.evalStatement: " ++ show s

  
evalPascalProc :: ProcedureInstance -> [Value] -> HEval ()
evalPascalProc procSig params = 
  do let ProcInstance sig block  = procSig
         Block decls stmt (sd:_) = block
     
     valT'   <- substParams params sig
     
     evalNewScope sd $ do putHeadValT valT'
                          evalDeclarations decls
                          evalStatement stmt


evalHaskellProc :: Procedure -> [Value] -> HEval ()
evalHaskellProc (HaskellProc _ f) vs = f vs
evalHaskellProc _ _ = error "Monad.evalHaskellProc"


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
   
    FunctionCall ident exprs sigPos ->
      do func   <- getFunction ident
         params <- mapM evalExpr exprs
         case func of
          (Function sigs)     -> evalPascalFunc ident (sigs !! sigPos) params
          (HaskellFunc _ _ _) -> evalHaskellFunc func params
    
    _         -> error $ "Eval.Monad.evalExpr: " ++ show expr
    
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
    _           -> error $ "Monad.evalConstant" ++ show c


evalPascalFunc :: Identifier -> FunctionInstance -> [Value] -> HEval Value
evalPascalFunc fId funcSig params =
  do let FuncInstance sig block t = funcSig
         Block decls stmt (sd:_)  = block
     
     paramsValT <- substParams params sig
     
     let symT'       = union (stSymT sd) (fromList [(fId, t)])
         paramsValT' = insert fId (defVal t) paramsValT
     
     evalNewScope sd $ do putHeadSymT symT'
                          putHeadValT paramsValT'
                          evalDeclarations decls
                          evalStatement stmt
                          getVarValue fId


evalHaskellFunc :: Function -> [Value] -> HEval Value
evalHaskellFunc (HaskellFunc _ _ f) vs = f vs
evalHaskellFunc _ _ = error "Eval.Monad.evalHaskellFunc"


substParams :: [Value] -> [Parameter] -> HEval ValueTable
substParams vals params = match vals params empty
 where
  -- fim
  match [] [] t = return t
  
  -- passando pra outra secao
  match vs ((Parameter _ [] _ _):ps) t =
    match vs ps t
  
  -- usando parametro default
  match [] ((Parameter _ [ident] _ (Just expr)):ps) t =
    do val <- evalExpr expr
       match [] ps (insert ident val t)

 -- usando argumento
  match (v:vs) ((Parameter m (ident:idl) typeV ex):ps) t =
    match vs ((Parameter m idl typeV ex):ps) (insert ident v t)
  
  match _ _ _ = error "Eval.Monad.substParams"


debug :: String -> HEval ()
debug msg = liftIO $ putStrLn $ "[EvalMonad] " ++ msg
