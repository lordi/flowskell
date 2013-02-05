module Flowskell.Lib.Color where
import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace
import Language.Scheme.Types
import Data.Array
import Flowskell.SchemeUtils

-- |Return a color triplet based on hue (in degrees) and, optionally, value and
-- saturation
doHSV :: [LispVal] -> IO (LispVal)
doHSV [h] = return $ makeFloatVector [r, g, b]
    where RGB r g b = hsv (extractNum h) 1 1
doHSV [h, s, v] = return $ makeFloatVector [r, g, b]
    where RGB r g b = hsv (extractNum h) (extractNum s) (extractNum v)

colorIOPrimitives = [ ("hsv", doHSV) ]
