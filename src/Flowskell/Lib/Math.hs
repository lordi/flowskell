module Flowskell.Lib.Math where
import Language.Scheme.Types
import Data.Array
import Flowskell.SchemeUtils

doVMul :: [LispVal] -> IO (LispVal)
doVMul [f, Vector v] = \
    return $ makeFloatVector (map mul (elems v))
    where mul f' = (extractFloat f) * (extractFloat f')

doVSum :: [LispVal] -> IO (LispVal)
doVSum [Vector v] = \
    return $ Float (sum (map extractFloat (elems v)))

doVLen :: [LispVal] -> IO (LispVal)
doVLen [Vector v] = \
    return $ Float (sqrt (sum (map square (elems v))))
    where square f = (extractFloat f) * (extractFloat f)

doVAdd :: [LispVal] -> IO (LispVal)
doVAdd [Vector a, Vector b] = \
    return $ makeFloatVector (map add el)
    where add (a', b') = (extractFloat a') + (extractFloat b')
          el = zip (elems a) (elems b)
doVAdd [f, Vector v] = \
    return $ makeFloatVector (map add (elems v))
    where add f' = (extractFloat f) + (extractFloat f')

mathIOPrimitives = [
        ("vmul", doVMul),
        ("vadd", doVAdd),
        ("vlen", doVLen),
        ("vsum", doVSum)
    ]
