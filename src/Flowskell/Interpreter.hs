module Flowskell.Interpreter where
import Control.Monad
import Control.Monad.Error

import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Parser
import Language.Scheme.Variables

import Flowskell.Lib.GL (glIOPrimitives)
import Flowskell.Lib.Random (randomIOPrimitives)
import Flowskell.Lib.Time (timeIOPrimitives)

primitives :: [ ((String, String), LispVal) ]
primitives = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) other
                where makeThrowErrorFunc f obj = liftIO $ f obj
                      other = timeIOPrimitives ++ glIOPrimitives ++ randomIOPrimitives

initSchemeEnv filename = do
  source <- readFile filename
  env <- primitiveBindings >>= flip extendEnv primitives

  let exprs = extractValue $ readExprList $ source in do
    forM exprs $ runIOThrows . liftM show . evalLisp env
    forM exprs $ putStrLn . show

  return env

evalFrame env = do
  evalString env "(every-frame)" >>= putStr
