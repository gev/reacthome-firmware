{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Implementation.Soundbox where


import           Control.Monad.Reader   (MonadReader)
import           Control.Monad.State    (MonadState)
import           Core.Context
import qualified Core.Domain            as D
import           Device.GD32F4xx.I2STRX
import           Feature.Lanamp
import qualified Feature.SRC4392        as S
import           GHC.TypeLits
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import qualified Interface.I2C          as I
import           Ivory.Language
import           Ivory.Stdlib

data Soundbox = Soundbox {
       src4392 :: S.SRC4392
    ,  lanamp  :: Lanamp 20 20
    }

soundbox :: (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 2, Output o, Pull p u) =>
            (p -> m (I2STX 20)) -> (p -> m (I2SRX 20)) -> (p -> m (i 2)) -> (p -> u -> m o) -> m Soundbox
soundbox i2sTx i2sRx i2c shutdown= do
    src4392 <- S.mkSRC4392 i2c shutdown
    lanamp  <- mkLanAmp i2sTx i2sRx
    pure Soundbox { lanamp, src4392 }
