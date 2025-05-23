{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.SRC4392 where


import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Context
import qualified Core.Domain          as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Matrix
import           Data.Value
import qualified Interface.I2C        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib




data SRC4392 = forall i. I.I2C i 2 => SRC4392
    { i2c      :: i 2
    , address  :: Uint8
    , config   :: Matrix 10 2 Uint8
    , tempBuff :: Buffer 2 Uint8
    , count    :: Value Uint8
    }

mkSRC4392 :: (MonadState Context m, MonadReader (D.Domain p c) m, I.I2C i 2)
          => (p -> m (i 2)) -> m SRC4392
mkSRC4392 i2c' = do
    mcu      <- asks D.mcu
    i2c      <- i2c' $ peripherals mcu
    tempBuff <- buffer "src4392"
    count    <- value "src4392_count" 0
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

    let src4392 = SRC4392 {i2c, address = 0xe0, config, tempBuff, count}

    addTask $ delay 10 "src4392_init" $ initSrc4392 src4392

    addHandler $ I.HandleI2C i2c $ \_ _ -> pure ()

    pure src4392

initSrc4392 SRC4392{..} = do
    count' <- deref count
    when (count' <? 10) $ do
        arrayMap $ \ix -> store (tempBuff ! ix) =<< deref (config ! toIx count' ! ix)
        I.transmit i2c address tempBuff
        store count $ count' + 1
