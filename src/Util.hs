module Util where

import           Ivory.Language


(.!) :: (RuntimeCast from a, IvoryBits a, IvoryBits from) => from -> from -> a
x .! n = castDefault (x `iShiftR` n * 8) .& 0xff
