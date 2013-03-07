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

-- |Try to evaluate LispVal, call catch function if it fails
try :: Env -> LispVal -> (LispError -> IO a) -> IO ()
try env lisp catch =
    evalLisp' env lisp >>= \x -> case x of
        Left error -> catch error >> return ()
        _ -> return ()

initSchemeEnv extraPrimitives filename = do
  let fskPrimitives = mkPrimitiveList $ defaultPrimitives ++ extraPrimitives
  fskLib <- getDataFileName "lib/flowskell.scm"
  env <- r5rsEnv >>= flip extendEnv fskPrimitives
  mapM_ (\file -> do
    try env (List [Atom "load", String file]) $ \ error -> do
        putStrLn $ "Failure in " ++ file ++ ":\n" ++ (show error))
        [fskLib, filename]
  return env

evalFrame env = do
  try env (List [Atom "every-frame-entry-point"]) $ \ error -> do
    print $ error
    evalString env "(define *has-error* #t)"
