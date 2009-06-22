-- | Modulo principal do interpretador HPascal.
-- Responsavel pelas acoes baixo nivel, presentes na Monad IO.

module Main where

import Eval.Monad (eval)
import Parser.HParser (parse)
import Parser.State (compErrors, ParserState)

import Control.Monad (when, liftM)

import System (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeFileName)


-- | Responsavel pela interface com o sistema externo.
-- Abre um arquivo com codigo fonte e o interpreta, eventualmente
-- mostrando resultados na saida padrao.
main :: IO ()
main = 
  do putStrLn " ___________________________________________ "
     putStrLn "|                                           |"
     putStrLn "|       HPascal interpreter (DIM0437)       |"
     putStrLn "|___________________________________________|"
     putStrLn ""
    
     args <- getArgs
    
     when (length args /= 1) $
        do putStrLn "Missing source filename"
           exitFailure

     let srcFilePath = head args
         srcName     = takeFileName srcFilePath

     src <- readFile srcFilePath
 
     let result = parse srcName src
 
     b <- liftM not (showParse result)
     when (b) exitFailure
      
     putStrLn ""
     eval (fromRight result)


-- | Imprime o resultado do processo de parsing
showParse :: (Show a, Show b) => Either a (b, ParserState) -> IO Bool

showParse (Left err) =
  do putStrLn "-> Parse Error:\n"
     print err
     return False
      
showParse (Right (tree, state)) = 
  do putStr "-> Parse tree:\n\n"
     print tree
     case compErrors state of
      Nothing   -> return True
      Just errL -> putStrLn ('\n': errL) >> return False

fromRight :: Either a (b, c) -> b
fromRight (Right (t,_)) = t
fromRight _ = error "Main.fromRight"
