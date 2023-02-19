{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

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

data Domain mcu t where
     Domain :: (I.MCU mcu, Transport t)
            => { model     :: Value  Uint8
               , version   :: V.Version
               , mac       :: M.Mac
               , mcu       :: mcu
               , transport :: t
               , features  :: [Feature]
               } -> Domain mcu t


domain :: (I.MCU mcu, Transport t)
       => Uint8
       -> (Uint8, Uint8)
       -> mcu
       -> t
       -> [Feature]
       -> Domain mcu t
domain model (major, minor) mcu transport features = Domain
    { model     = value "model" model
    , version   = V.version "version" major minor
    , mac       = I.mac mcu "mac"
    , mcu       = mcu
    , transport = transport
    , features  = features
    }



instance Include (Domain mcu t) where
    include (Domain {model, version, mac, transport}) = do
        include model
        include version
        include mac
        include transport


instance Initialize (Domain mcu t) where
    initialize (Domain {mac, transport}) =
        initialize mac <> initialize transport
