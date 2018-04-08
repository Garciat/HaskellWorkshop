{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types


foreign import
  ccall "math.h sin"
  c_sin :: CDouble -> CDouble


main = print (c_sin (pi / 2))
