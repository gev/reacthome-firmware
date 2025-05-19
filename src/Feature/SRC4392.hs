{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Feature.SRC4392 where

import           Control.Monad.RWS
import           Core.Context
import qualified Core.Domain         as D
import           Data.Buffer
import           Data.Matrix
import           Data.Value
import qualified Interface.I2C       as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Uint 

data SRC4392 = forall i. I.I2C i 2 => SRC4392
    { i2c      :: i 2
    , address  :: Value Uint8
    , config   :: Matrix 10 2 Uint8
    , tempBuff :: Buffer 2 Uint8
    }

mkSRC4392 :: (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 2)
          => (p -> m (i 2)) -> m SRC4392
mkSRC4392 i2c' = do
    mcu      <- asks D.mcu
    i2c      <- i2c' $ peripherals mcu
    address  <- value  "src4392_address" 0xc0
    tempBuff <- buffer "src4392"
    config   <- matrix "src4392_config" [[0x7f, 0x00],
                                         [0x01, 0x3F],
                                         [0x03, 0x31],
                                         [0x0d, 0x08],
                                         [0x0e, 0x10],
                                         [0x0f, 0x12],
                                         [0x10, 0x00],
                                         [0x11, 0x00],
                                         [0x2d, 0x02],
                                         [0x2f, 0x00]]

    let src4392 = SRC4392 {i2c, address, config, tempBuff}

    addInit "src4392_init" $ do
        address' <- deref address
        arrayMap $ \jx -> do
            arrayMap $ \ix -> store (tempBuff ! toIx ix) =<< deref (config ! jx ! ix)
            I.transmit i2c address' tempBuff

    pure src4392


