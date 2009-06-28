--| Modulo que visa a exibir, em modo amigavel,
-- a arvore sintatica correpondente ao programa analisado.
module Language.ShowTree where

import Language.AST
import Language.Basic

-- | Funcao que retorna uma String representativa da arvore
-- sintatica utilizada para modelar um dado programa.
showParseTree :: Program -> String
showParseTree = showProgram
 where showProgram (Program name uses block) =
        "Program " ++ show name ++ " "
                   ++ show uses ++ "\n"
                   ++ showBlock 1 block
        
       showBlock n (Block d stmt sd) =
        pad n ++ "Block" ++
        "\n" ++ show sd  ++
        "\n" ++ showDeclPart (n+1) d ++
        "\n" ++ pad n ++ showStmt (n+1) stmt
       
       showDeclPart n (DeclPart _ _ vds pds) =
        pad n ++ "DeclarationPart" ++
        "\n" ++ showPadList (n+1) vds showVarDecl ++
        "\n" ++ pad n ++ show pds
        
       showVarDecl n (VarDec idl typeV mExpr) =
        pad n ++ "VarDeclaration [" ++ (concat (intersperse " " idl))
        ++ "] " ++ show typeV
        ++ " " ++ "(" ++ show mExpr ++ ")"
       
       showStmt n (Nop) = pad n ++ "Nop"
       
       showStmt n (Assignment var expr) = pad n ++
        "Assignment " ++ show var ++ " " ++ show expr
       
       showStmt n (Compound stmtl) = pad n ++
        "Compound\n" ++ showPadList (n+1) stmtl showStmt
       
       showStmt n (If expr st1 st2) = pad n ++
        "If " ++ show expr ++ "\n" ++ showStmt (n+1) st1
        ++ "\n" ++ showStmt (n+1) st2
       
       showStmt n (For up ide e1 e2 stmt) = pad n ++
        "For " ++ show up ++ " " ++ show ide ++ " (" ++
        show e1 ++ ") (" ++ show e2 ++ ")\n" ++ showStmt (n+1) stmt
       
       showStmt n (Repeat stmt expr) = pad n ++
        "Repeat " ++ show expr ++ "\n" ++ showStmt (n+1) stmt
       
       showStmt n (While expr stmt) = pad n ++
        "While " ++ show expr ++ "\n" ++ showStmt (n+1) stmt
       
       showStmt n x = pad n ++ show x 

       showPadList n ls f =
        pad n ++ "[" ++ "\n" ++
          (concat (intersperse "\n" (map (f (n+1)) ls)))
          ++ "\n" ++ pad n ++ "]"
          
       pad = (`replicate` ' ')
