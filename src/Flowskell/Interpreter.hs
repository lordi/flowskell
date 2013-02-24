module Flowskell.Interpreter where
import Control.Monad
import Control.Monad.Error

import Language.Scheme.Core (r5rsEnv, evalString, evalLisp', primitiveBindings)
import Language.Scheme.Types
import Language.Scheme.Util (escapeBackslashes)
import Language.Scheme.Parser
import Language.Scheme.Variables (extendEnv)

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
  let fskPrimitives = mkPrimitiveList $ defaultPrimitives ++ extraPrimitives
  fskLib <- getDataFileName "lib/flowskell.scm"
  env <- r5rsEnv >>= flip extendEnv fskPrimitives
  mapM_ (\file -> do
    result <- evalString env $ "(load \"" ++ escapeBackslashes file ++ "\")"
    putStrLn $ file ++ ": " ++ result) [fskLib, filename]
  return env

evalFrame env = do
  evalLisp' env (List [Atom "every-frame-entry-point"]) >>= \x -> case x of
    Left error -> do
        print $ error
        evalString env "(define *has-error* #t)"
        return ()
    _ -> return ()
