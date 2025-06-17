{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Implementation.Soundbox where


import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import           Core.Context
import qualified Core.Domain           as D
import           Device.GD32F4xx.I2STRX
import           Feature.Lanamp
import qualified Feature.SRC4392       as S
import           GHC.TypeLits
import qualified Interface.I2C         as I
import           Ivory.Language
import           Ivory.Stdlib
import           Interface.GPIO.Output
import           Interface.GPIO.Port

data Soundbox = Soundbox {
       src4392 :: S.SRC4392
    ,  lanamp  :: Lanamp 20 20
    }

soundbox :: (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 2, Output o, Pull p u) =>
            (p -> m (I2STRX 20 20)) -> (p -> u -> m o) -> (p -> m (i 2)) -> (p -> u -> m o) -> m Soundbox
soundbox i2sTrx shutdown i2c mute = do
    src4392 <- S.mkSRC4392 i2c mute
    lanamp  <- mkLanAmp i2sTrx shutdown
    pure Soundbox { lanamp, src4392 }