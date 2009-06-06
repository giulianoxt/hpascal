module Main where

import Parsing (hparser)
import ParserRun (parse, defaultSymbols)

import System (getArgs)
import Control.Monad (when)
import System.Exit (exitFailure)
import System.FilePath (takeFileName)


main :: IO ()
main = 
  do putStrLn "HPascal interpreter v0.1\n"
    
     args <- getArgs
    
     when (length args /= 1) $
        do putStrLn "Missing source filename"
           exitFailure

     let srcFilePath = head args
         srcName     = takeFileName srcFilePath

     src <- readFile srcFilePath
 
     let result = parse hparser defaultSymbols srcName src
 
     showParse result


showParse (Left err)   =
  do putStr "Parse Error:"
     print err
     
showParse (Right (tree, state)) = 
  do putStr "Parse tree: "
     print tree
     putStr "\nFinal Parser State: "
     print state
