module Flowskell.Interpreter where
import Control.Monad
import Control.Monad.Error

import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Parser
import Language.Scheme.Variables
import Language.Scheme.Compiler

import Flowskell.Lib.GL (glIOPrimitives)
import Flowskell.Lib.Random (randomIOPrimitives)
import Flowskell.Lib.Time (timeIOPrimitives)
import Flowskell.Lib.Color (colorIOPrimitives)

import Paths_Flowskell

primitives :: [ ((String, String), LispVal) ]
primitives = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) other
                where makeThrowErrorFunc f obj = liftIO $ f obj
                      other = timeIOPrimitives ++ glIOPrimitives ++ randomIOPrimitives ++ colorIOPrimitives

initSchemeEnv filename = do
  stdlib <- getDataFileName "lib/stdlib.scm"
  stdlib2 <- getDataFileName "lib/flowskell.scm"
  env <- primitiveBindings >>= flip extendEnv primitives
  let files = [stdlib, stdlib2, filename]
  evalString env $ "(define *source* \"" ++ (escapeBackslashes filename) ++ "\")"
  mapM (\file -> do
    result <- evalString env $ "(load \"" ++ (escapeBackslashes file) ++ "\")"
    putStrLn $ file ++ ": " ++ result) files

  -- x <- liftIO $ extractValue $ evalLisp env $ "*source*"
  -- putStrLn $ show x
  return env

evalFrame env = do
  evalString env "(every-frame)" >>= putStr
