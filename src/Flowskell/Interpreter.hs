module Flowskell.Interpreter (initSchemeEnv, initPrimitives, evalFrame) where
import Data.Maybe (isJust)

import Graphics.Rendering.OpenGL hiding (Bool, Float) -- get, $=

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
import Flowskell.State

#ifdef USE_JACK
import Flowskell.Lib.Jack (initJack)
#endif
#ifdef USE_TEXTURES
import Flowskell.Lib.Textures (initTextures)
#endif
#ifdef RENDER_TO_TEXTURE
import Flowskell.Lib.Shaders (initShaders)
#endif

import Paths_Flowskell

defaultPrimitives = timeIOPrimitives ++ glIOPrimitives ++ randomIOPrimitives ++ colorIOPrimitives ++ mathIOPrimitives

mkPrimitiveList b = map (\(n, f) -> (('v', n), CustFunc $ makeThrowErrorFunc f)) b
                where makeThrowErrorFunc f obj = liftIO $ f obj

-- |Try to evaluate LispVal, call catch function if it fails
tryEval :: Env -> LispVal -> (LispError -> IO a) -> IO ()
tryEval env lisp catch =
    evalLisp' env lisp >>= \x -> case x of
        Left error -> catch error >> return ()
        _ -> return ()

reportError :: String -> LispError -> IO ()
reportError file err = putStrLn $ "Failure in " ++ file ++ ":\n" ++ (show err)

initSchemeEnv extraPrimitives filename = do
  let fskPrimitives = mkPrimitiveList $ defaultPrimitives ++ extraPrimitives
  fskLib <- getDataFileName "lib/flowskell.scm"
  env <- r5rsEnv >>= flip extendEnv fskPrimitives
  mapM_ (\f -> tryEval env (List [Atom "load", String f]) $ reportError f)
        [fskLib, filename]
  return env

evalFrame state env =
  tryEval env (List [Atom "every-frame-entry-point"]) $ \ error -> do
    reportError "<source>" error
    maybeLastEnv <- get $ lastEnvironment state
    when (isJust maybeLastEnv) $ environment state $= maybeLastEnv

initPrimitives state = do
#ifdef RENDER_TO_TEXTURE
  shaderIOPrimitives <- initShaders state
#else
  shaderIOPrimitives <- return []
#endif

#ifdef USE_TEXTURES
  texturesIOPrimitives <- initTextures state
#else
  texturesIOPrimitives <- return []
#endif

#ifdef USE_JACK
  jackIOPrimitives <- initJack
#else
  jackIOPrimitives <- return []
#endif

  return $ shaderIOPrimitives ++ texturesIOPrimitives ++ jackIOPrimitives

