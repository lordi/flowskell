module Flowskell.Lib.Time where
import Data.Time.Clock
import Data.Time.Calendar
import Language.Scheme.Types

time = getCurrentTime >>= return . utctDayTime
timeInSeconds [] = do x <- time; return $ Float $ realToFrac x
timeInMilliSeconds [] = do x <- time; return $ Float $ 1000 * (realToFrac x)

timeIOPrimitives = [
                -- TODO "secs" should be a variable, not a funcion?! or at
                -- least memoized
                   ("secs", timeInSeconds),
                   ("msecs", timeInMilliSeconds)
                 ]
