module Flowskell.Lib.Math where
import Language.Scheme.Types
import Data.Array

doVMul :: [LispVal] -> IO (LispVal)
doVMul [Float f, Vector v] = \
    return $ Vector ((listArray (0, (length el) - 1)) (map mul el))
    where mul (Float f') = Float (f * f')
          el = elems v

doVSum :: [LispVal] -> IO (LispVal)
doVSum [Vector v] = \
    return $ Float (sum (map extr el))
    where extr (Float f') = f'
          el = elems v

doVLen :: [LispVal] -> IO (LispVal)
doVLen [Vector v] = \
    return $ Float (sqrt (sum (map square el)))
    where square (Float f) = f * f
          el = elems v

doVAdd :: [LispVal] -> IO (LispVal)
doVAdd [Float f, Vector v] = \
    return $ Vector ((listArray (0, (length el) - 1)) (map add el))
    where add (Float f') = Float (f + f')
          el = elems v
doVAdd [Vector a, Vector b] = \
    return $ Vector ((listArray (0, (length el) - 1)) (map add el))
    where add ((Float a'), (Float b')) = Float (a' + b')
          el = zip (elems a) (elems b)

mathIOPrimitives = [
        ("vmul", doVMul),
        ("vadd", doVAdd),
        ("vlen", doVLen),
        ("vsum", doVSum)
    ]
