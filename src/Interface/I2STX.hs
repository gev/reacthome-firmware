{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.I2STX where

import Core.Context
import Core.Handler
import Data.Buffer
import Data.Record
import GHC.TypeNats
import Interface.I2S
import Ivory.Language
import Ivory.Language.Module

data HandleI2STX i = HandleI2STX
  { i2s :: i
  , -- , handle :: forall n eff. Uint32 -> forall eff. Ivory eff ()
    handle :: forall eff. Ivory eff Sample
  }

{-
     Implementation of I2STX
-}
-- buff0 :: Buffer n Uint32
-- buff1 :: Buffer n Uint32

-- index = 0

-- ptr :: Array 2 (Ptr (Array n Uint32))
-- ptr = [buff0, buff1]

-- ifte_ index ?= 0
--     .... ptr ! index
--     store index (1 - index)
--     handle (ptr ! index)

-- {-
--     Feature
-- -}
-- buff :: Buffer n Uint32

-- handle :: (Buffer n Uint32 -> Ivory eff ()) -> Ivory eff ()

-- handle buff' = do
--     arrayMap \i -> do
--         store (buff' ! i) =<< deref (buff ! i)
