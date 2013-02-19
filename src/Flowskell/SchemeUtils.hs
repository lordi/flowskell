module Flowskell.SchemeUtils where
import Language.Scheme.Types
import Data.Array

extractFloat (Float n) = realToFrac n
extractFloat (Number n) = realToFrac n
extractFloat (Rational n) = realToFrac n

makeFloatVector lst = Vector $ listArray (0, (length lst) - 1) (map Float lst)
