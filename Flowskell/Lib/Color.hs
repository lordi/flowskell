module Flowskell.Lib.Color where
import Data.Colour.RGBSpace.HSV
import Language.Scheme.Types

hsv :: (RealFrac a, Ord a) => a -> a -> a -> RGB aSource
doHSV [Float h, Float s, Float v] = return List [(channelRed rgb)]
                            where rgb = hsv h s v


colorIOPrimitives = [ ("hsv", doHSV) ]
