module Flowskell.Lib.Random where
import System.Random
import Language.Scheme.Types

doRandom :: [LispVal] -> IO LispVal
doRandom [Number n] = newStdGen >>= return . fst . randomR (0, n) >>= return . Number

randomIOPrimitives = [ ("random", doRandom) ]
