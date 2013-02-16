module Flowskell.Lib.Shaders where
import Prelude hiding ( sum )
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Foldable ( Foldable, sum )
import Data.IORef
import Graphics.UI.GLUT

import Language.Scheme.Types
import Flowskell.State (State(..))
import Flowskell.SchemeUtils (extractFloat)

doBlur :: State -> [LispVal] -> IO LispVal
doBlur state [n] = do
    blurFactor state $= (extractFloat n)
    return (Number 1)

initShaders :: State -> IO [(String, [LispVal] -> IO LispVal)]
initShaders state = do
    return  [
        ("blur", doBlur state)
      ]

