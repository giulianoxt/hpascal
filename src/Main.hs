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
 
     case parse hparser defaultSymbols srcName src of
       Left err   -> putStrLn "Parse error:" >> print err
       Right tree -> putStrLn "Parse tree:"  >> print tree
