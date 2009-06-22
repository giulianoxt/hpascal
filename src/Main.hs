-- | Modulo principal do interpretador HPascal.
-- Responsavel pelas acoes baixo nivel, presentes na Monad IO.

module Main where

import Eval.Monad (eval)
import Parser.HParser (parse)

import Control.Monad (when)

import System (getArgs)
import System.Console.GetOpt
import System.Exit
import System.FilePath (takeFileName)


data Flag =
   Version
 | PrintParseTree


options :: [OptDescr Flag]
options = [
    Option ['v'] ["version"] (NoArg Version)
      "print usage"
  , Option ['p'] ["show-parse-tree"] (NoArg PrintParseTree)
      "show produced parse tree, even with errors"
 ]


data Preferences = Preferences {
   showTree :: Bool
 }


initialPrefs :: Preferences
initialPrefs = Preferences {
   showTree = False
 }


-- | Responsavel pela interface com o sistema externo.
-- Abre um arquivo com codigo fonte e o interpreta, eventualmente
-- mostrando resultados na saida padrao.
main :: IO ()
main = 
  do (srcFilePath, prefs) <- handleArgs initialPrefs
     
     -- Parsing
     
     let srcName = takeFileName srcFilePath
     
     src <- readFile srcFilePath
     
     let parseResult = parse srcName src
     
     b <- showParse parseResult (showTree prefs)
     
     when (not b) exitFailure
     
     
     -- Eval
     
     eval (fromRight parseResult)


handleArgs :: Preferences -> IO (String, Preferences)
handleArgs prefs =
  do args <- getArgs
     case getOpt RequireOrder options args of
      (flags, [src], []) -> check flags prefs >>= return . ((,) src)
      (_,     _,   msgs) -> failure $ unwords msgs
 where
    check :: [Flag] -> Preferences -> IO Preferences
    check [] pfs                  =
      return pfs
    check (Version:_) _           =
      putStrLn header >> exitSuccess
    check (PrintParseTree:fs) pfs =
      check fs $ pfs { showTree = True }
      

failure :: String -> IO a
failure msg =
  do putStrLn (header ++ msg)
     exitFailure


header :: String
header = "HPascal Interpreter - DIM0437\n"
      ++ "Usage: hpascal [-v|--version] [-p|--show-parse-tree] filename\n"


showParse :: (Show a) => Either String a -> Bool -> IO Bool
showParse (Left  s)  _ =
  do putStrLn s
     return False
showParse (Right p) sh = 
  do when sh $ putStrLn (show p)
     return True


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "Main.fromRight"
