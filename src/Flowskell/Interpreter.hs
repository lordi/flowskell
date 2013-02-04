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
import Flowskell.Lib.Math (mathIOPrimitives)

import Paths_Flowskell

defaultPrimitives = timeIOPrimitives ++ glIOPrimitives ++ randomIOPrimitives ++ colorIOPrimitives ++ mathIOPrimitives

mkPrimitiveList b = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) b
                where makeThrowErrorFunc f obj = liftIO $ f obj

initSchemeEnv extraPrimitives filename = do
  stdlib <- getDataFileName "lib/stdlib.scm"
  stdlib2 <- getDataFileName "lib/flowskell.scm"
  let primitives = mkPrimitiveList $ defaultPrimitives ++ extraPrimitives
  env <- primitiveBindings >>= flip extendEnv primitives
  let files = [stdlib, stdlib2, filename]
  evalString env $ "(define *source* \"" ++ (escapeBackslashes filename) ++ "\")"
  mapM (\file -> do
    result <- evalString env $ "(load \"" ++ (escapeBackslashes file) ++ "\")"
    putStrLn $ file ++ ": " ++ result) files

  return env

evalFrame env = do
  evalLisp' env (List [Atom "every-frame-entry-point"]) >>= \x -> case x of
    Left error -> do
        putStrLn $ show error
        evalString env $ "(define *has-error* #t)"
        return ()
    _ -> return ()
