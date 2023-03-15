{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

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



data Domain p t where
     Domain :: Transport t
            => { model      :: Value  Uint8
               , version    :: V.Version
               , mcu        :: I.MCU p
               , shouldInit :: Value IBool
               , transport  :: t
               , features   :: [Feature]
               } -> Domain p t



domain :: Transport t
       => Uint8
       -> (Uint8, Uint8)
       -> I.MCU p
       -> IBool
       -> t
       -> [Feature]
       -> Domain p t
domain model (major, minor) mcu shouldInit transport features = Domain
    { model      = value "model" model
    , version    = V.version "version" major minor
    , mcu        = mcu
    , shouldInit = value "should_init" shouldInit
    , transport  = transport
    , features   = features
    }



instance Include (Domain p t) where
    include (Domain {..}) = do
        inclCast
        inclSerialize
        include mcu
        include model
        include version
        include shouldInit
        include transport



instance Initialize (Domain p t) where
    initialize (Domain {..}) =
        initialize mcu <> initialize transport
