module Flowskell.Lib.Color where
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Language.Scheme.Types
import Data.Array

doHSV :: [LispVal] -> IO (LispVal)
doHSV [Float h] = return $ Vector ((listArray (0, 2)) [Float r, Float g, Float b])
    where RGB r g b = hsv h 1.0 1.0
doHSV [Float h, Float s, Float v] = return $ Vector ((listArray (0, 2)) [Float r, Float g, Float b])
    where RGB r g b = hsv h s v

colorIOPrimitives = [ ("hsv", doHSV) ]
