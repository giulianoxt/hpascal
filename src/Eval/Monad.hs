
module Eval.Monad where

import Eval.Values
import Eval.Runtime
import Language.AST
import Language.Basic

import Data.Maybe
import Data.Map hiding (map)

import Control.Monad.State


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

evalStatement (Assignment varId expr) =
  do value <- evalExpr expr
    
     varD  <- getVarDescriptor varId
     
     case isReference varD of
      Nothing -> insertVal varId value
      Just r  -> insertRefVal r value

evalStatement (FunctionReturn fId expr) =
  evalStatement (Assignment fId expr)
 
evalStatement (ProcedureCall ident exprs sigPos) =
  do proc    <- getProcedure ident
     case proc of
      (Procedure sigs)  -> evalPascalProc (sigs !! sigPos) exprs
      (HaskellProc _ _) -> evalHaskellProc proc exprs

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

evalStatement (For update' ident e1 e2 stmt) =
  do v1 <- evalExpr e1
     v2 <- evalExpr e2
     evalFor v1 v2
 where
  evalAssign e = evalStatement (Assignment ident e)
 
  evalFor (IntVal a) vb@(IntVal b) =
    let go f = do evalAssign $ ConstExpr $ ConstNum $ IntNum a
                  evalStatement stmt
                  evalFor (IntVal (f a)) vb in
    case update' of
      To     | a <= b -> go (\x -> x + 1)  -- (+1) n pega
      Downto | a >= b -> go (\x -> x - 1)  -- nem (-1). wtf?
      _               -> return ()
   
  evalFor _ _ = error "Eval.Monad.evalStatement.evalFor"


evalStatement s = error $ "Eval.Monad.evalStatement: " ++ show s


evalPascalProc :: ProcedureInstance -> [Expr] -> HEval ()
evalPascalProc procSig params = 
  do let ProcInstance sig block  = procSig
         Block decls stmt (sd:_) = block
     
     rParams       <- runtimeParameters params
     
     (refT, valT') <- substParams rParams sig
     
     evalNewScope sd $ do putHeadValT valT'
                          mergeRefTable refT
                          evalDeclarations decls
                          evalStatement stmt


evalHaskellProc :: Procedure -> [Expr] -> HEval ()
evalHaskellProc (HaskellProc _ f) exprs =
  do params <- mapM evalExpr exprs
     f params
evalHaskellProc _ _ = error "Monad.evalHaskellProc"


evalExpr :: Expr -> HEval Value
evalExpr expr =
  case expr of
    e1 :=: e2  -> bin e1 e2 eqOp2
    e1 :<: e2  -> bin e1 e2 $ relOp2 (<) (<)
    e1 :>: e2  -> bin e1 e2 $ relOp2 (>) (>)
    e1 :<=: e2 -> bin e1 e2 $ relOp2 (<=) (<=)
    e1 :>=: e2 -> bin e1 e2 $ relOp2 (>=) (>=)
    e1 :<>: e2 -> bin e1 e2 $ relOp2 (/=) (/=)
    
    e1 :+: e2 -> bin e1 e2 $ numOp2 (+) (+)
    e1 :-: e2 -> bin e1 e2 $ numOp2 (-) (-)
    e1 :*: e2 -> bin e1 e2 $ numOp2 (*) (*)
    e1 :/: e2 -> bin e1 e2 $ divOp2
    
    Not e     -> unary e   $ boolOp1 not
    Var varR  -> evalVarReference varR
    
    ConstExpr c -> evalConstant c
   
    FunctionCall ident exprs sigPos ->
      do func   <- getFunction ident
         case func of
          (Function sigs)     -> evalPascalFunc ident (sigs !! sigPos) exprs
          (HaskellFunc _ _ _) -> evalHaskellFunc func exprs
    
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
    ConstNum (IntNum i)   -> return (IntVal i)
    ConstNum (FloatNum f) -> return (FloatVal f)
    ConstBool b           -> return (BoolVal b)
    ConstChar x           -> return (CharVal x)
    ConstStr  s           -> return (StringVal s)
    _                     -> error $ "Monad.evalConstant" ++ show c


evalPascalFunc :: Identifier -> FunctionInstance -> [Expr] -> HEval Value
evalPascalFunc fId funcSig params =
  do let FuncInstance sig block t = funcSig
         Block decls stmt (sd:_)  = block
         
         oldSymT        = stSymT sd
         funcDescriptor = VarDescriptor t False Nothing
         symTWithFunc   = fromList [(fId, funcDescriptor)]
         newSymT        = union oldSymT symTWithFunc
     
     rParams       <- runtimeParameters params
     
     (refT, valT') <- substParams rParams sig
     
     let symT'       = newSymT
         paramsValT' = insert fId (defVal t) valT'
     
     evalNewScope sd $ do putHeadSymT symT'
                          putHeadValT paramsValT'
                          mergeRefTable refT
                          
                          evalDeclarations decls
                          evalStatement stmt
                          getVarValue fId


evalHaskellFunc :: Function -> [Expr] -> HEval Value
evalHaskellFunc (HaskellFunc _ _ f) exprs =
  do params <- mapM evalExpr exprs
     f params
evalHaskellFunc _ _ = error "Eval.Monad.evalHaskellFunc"


substParams :: [RuntimeParameter]
            -> [Parameter]
            -> HEval (Map Identifier Reference, ValueTable)
substParams vals params = match vals params (empty, empty)
 where
  -- fim
  match [] [] ts = return ts
  
  -- passando pra outra secao
  match vs ((Parameter _ [] _ _):ps) ts = match vs ps ts
  
  -- usando parametro default
  match [] ((Parameter _ [ident] _ (Just expr)):ps) (rt,vt) =
    do val <- evalExpr expr
       match [] ps (rt,(insert ident val vt))

  -- passando por referencia
  match (varP:vs) ((Parameter Reference (ident:idl) typeV ex):ps) (rt,vt) =
    do 
       let newParams = Parameter Reference idl typeV ex : ps
       case varP of
         ExprParameter _ -> error "subsParams: expression passed as reference"
         VarParameter _ (Just ref) _ _ -> 
           do let rt' = insert ident ref rt
              match vs newParams (rt',vt)
         VarParameter varId _ sc _   ->
           do let newRef = StackReference sc varId
                  rt'    = insert ident newRef rt
                  vt'    = insert ident (error "subsParams: reference value") vt
              match vs newParams (rt',vt')

 -- usando argumento
  match (varP:vs) ((Parameter m (ident:idl) typeV ex):ps) (rt,vt) =
    do 
       let newParams = Parameter m idl typeV ex : ps
           val       = case varP of
                         ExprParameter v      -> v
                         VarParameter _ _ _ v -> v
           vt'       = insert ident val vt
                         
       match vs newParams (rt,vt')
  
  match _ _ _ = error "Eval.Monad.substParams"


runtimeParameters :: [Expr] -> HEval [RuntimeParameter]
runtimeParameters = mapM singleParam
 where
  singleParam expr@(Var varId) =
    do varD     <- getVarDescriptor varId
       varScope <- getVarScope varId
       
       let mref = isReference varD
      
       let scope' = case mref of 
                     Nothing -> varScope
                     Just sr -> refScope sr
       
       val <- evalExpr expr
       return (VarParameter varId mref scope' val)
      
  
  singleParam expr =
    do val <- evalExpr expr
       return (ExprParameter val)


debug :: String -> HEval ()
debug msg = liftIO $ putStrLn $ "[EvalMonad] " ++ msg
