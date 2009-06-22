
module Eval.Monad where

import Eval.Runtime
import Language.AST

import Data.Map ()
import Control.Monad.State


type HEval a = StateT RuntimeEnv IO a


evalProgram :: Program -> HEval ()
evalProgram (Program _ _ block) = evalBlock block


evalBlock :: Block -> HEval ()
evalBlock (Block decls stmt sd) =
  do modify $ updateRuntimeEnv sd
     evalDeclarations decls
     evalStatement stmt

evalDeclarations :: DeclarationsPart -> HEval ()
evalDeclarations (DeclPart _ _ varL _) =
  forM_ varL evalVarDeclaration

evalVarDeclaration :: VariableDeclaration -> HEval ()
evalVarDeclaration (VarDec [varId] _ (Just expr)) =
  val <- evalExpression expr
  
  

evalVarDeclaration _ = return ()
  








eval :: Program -> IO ()
eval p = evalStateT (evalProgram p) defaultRuntimeEnv 
