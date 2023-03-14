{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Domain where

import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Transport
import qualified Core.Version          as V
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Interface.Mac         as M
import qualified Interface.MCU         as I
import           Interface.SystemClock
import           Ivory.Language
import           Support.Cast
import           Support.Serialize

data Domain mcu t where
     Domain :: (I.MCU mcu, Transport t)
            => { model      :: Value  Uint8
               , version    :: V.Version
               , mac        :: M.Mac
               , mcu        :: mcu
               , shouldInit :: Value IBool
               , transport  :: t
               , features   :: [Feature]
               } -> Domain mcu t


domain :: (I.MCU mcu, Transport t)
       => Uint8
       -> (Uint8, Uint8)
       -> mcu
       -> IBool
       -> t
       -> [Feature]
       -> Domain mcu t
domain model (major, minor) mcu shouldInit transport features = Domain
    { model      = value "model" model
    , version    = V.version "version" major minor
    , mac        = I.mac mcu "mac"
    , mcu        = mcu
    , shouldInit = value "should_init" shouldInit
    , transport  = transport
    , features   = features
    }



instance I.MCU mcu => Include (Domain mcu t) where
    include (Domain {..}) = do
        inclCast
        inclSerialize
        include mcu
        include model
        include version
        include mac
        include shouldInit
        include transport


instance Initialize (Domain mcu t) where
    initialize (Domain {..}) =
        initialize mac <> initialize transport
