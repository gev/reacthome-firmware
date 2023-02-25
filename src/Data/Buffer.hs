module Data.Buffer where

import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = Values n t


buffer :: (IvoryZeroVal t, IvoryInit t, KnownNat n) => String -> Buffer n t
buffer id = values_ $ id <> "_buffer"
