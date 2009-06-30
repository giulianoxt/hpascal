
module Modules.Basic where

import Language.AST
import Eval.Values
import TypeSystem.Types

import Control.Monad.Trans (liftIO)


-- * Utils


haskellProc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> ([Value] -> IO ())     -- ^ procedimento na monad IO
            -> Procedure
haskellProc c f   = HaskellProc c (liftIO . f)


haskellFunc :: ([Type] -> Bool)       -- ^ checagem da assinatura
            -> Type                   -- ^ tipo de retorno da funcao
            -> ([Value] -> IO Value)  -- ^ funcao na monad IO
            -> Function
haskellFunc c t f = HaskellFunc c t (liftIO . f)


pureHaskellFunc :: ([Type] -> Bool)   -- ^ checagem da assinatura
                -> Type               -- ^ tipo de retorno
                -> ([Value] -> Value) -- ^ funcao pura
                -> Function
pureHaskellFunc c t f = HaskellFunc c t (return . f)
