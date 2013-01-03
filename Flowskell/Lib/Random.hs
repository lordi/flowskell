module Flowskell.Lib.Random where
import Control.Monad
import System.Random
import Control.Monad.Error
import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Parser
import Language.Scheme.Variables

doRnd :: [LispVal] -> IO LispVal
doRnd [] = newStdGen >>= return . fst . randomR (0,1) >>= return . Float

makeThrowErrorFunc :: ([LispVal] -> IO LispVal) -> [LispVal] -> IOThrowsError LispVal
makeThrowErrorFunc f obj = do
    x <- liftIO $ f obj
    return x

randomPrimitives :: [ ((String, String), LispVal) ]
randomPrimitives = map (\(n, f) -> (("v", n), IOFunc $ makeThrowErrorFunc f)) [
                   ("rnd", doRnd)
                   ]
