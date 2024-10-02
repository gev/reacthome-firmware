{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Dopplers
    ( Dopplers
    , dopplers
    ) where



import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Fixed
import           Data.Value
import qualified Interface.ADC        as I
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib



data Doppler a = Doppler
    { adc         :: a
    , expectation :: Value IFloat
    , measurement :: Value IFloat
    , threshold   :: Value IFloat
    , current     :: Value Uint8
    , previous    :: Value Uint8
    , count       :: Value Uint32
    }



data Dopplers n = forall a t. (I.ADC a, T.LazyTransport t) => Dopplers
    { n         :: Uint8
    , doppler   :: List n (Doppler a)
    , transport :: t
    }



dopplers :: ( MonadState Context m
            , MonadReader (D.Domain p c) m
            , T.LazyTransport t
            , I.ADC a
            ) => List n (p -> m a) -> t -> m (Dopplers n)
dopplers analogInput transport = do
    mcu              <- asks D.mcu
    doppler          <- zipWithM (mkDoppler transport) analogInput nats

    let dopplers = Dopplers { n = fromIntegral $ length analogInput
                            , doppler
                            , transport
                            }

    addTask $ delay     1 "doppler_measure" $ mapM_ measure doppler
    addTask $ delay   200 "doppler_sync"    $ sync  dopplers
    addTask $ delay 15000 "doppler_reset"   $ reset dopplers

    pure dopplers



mkDoppler :: ( MonadState Context m
             , MonadReader (D.Domain p c) m
             , T.LazyTransport t
             , I.ADC a
             ) => t -> (p -> m a) -> Int-> m (Doppler a)
mkDoppler transport analogInput index = do
    let name          = "doppler_" <> show index <> "_"
    mcu              <- asks D.mcu
    let peripherals'  = peripherals mcu
    adc              <- analogInput peripherals'
    expectation      <- value (name <> "expectation") 0.5
    measurement      <- value (name <> "measurement")   0
    threshold        <- value (name <> "threshold"  )   level
    current          <- value (name <> "current"    )   0
    previous         <- value (name <> "previous"   )   0
    count            <- value (name <> "count"      )   0

    let doppler = Doppler { adc
                          , expectation
                          , measurement
                          , threshold
                          , current
                          , previous
                          , count
                          }

    pure doppler



measure :: I.ADC a => Doppler a -> Ivory (ProcEffects s ()) ()
measure Doppler {..} = do
    a' <- I.getReduced adc

    expectation' <- deref expectation

    measurement' <- deref measurement
    store measurement a'

    let derivative = a' - measurement'

    let diff  = abs $ a' - expectation'
    threshold' <- deref threshold
    when (diff <? level)  $
        store expectation $ average alpha expectation' a'
    when (diff <? level') $
        store threshold   $ average alpha threshold' diff

    let range = iMax expectation' $ 1 - expectation'
    let threshold'' = high * threshold'
    value <- local $ ival 0
    ifte_ (diff >? threshold'')
          (store value $ (diff - threshold'') / (range - threshold''))
          (
            when (derivative >? threshold' / 2 .|| diff >? threshold' * low) $ do
              count' <- deref count
              store count $ count' + 1
          )

    value' <- deref value
    count' <- deref count
    when (count' >? maxCount) $ store count 0
    current' <- deref current
    let current'' = castDefault $ count' .| castDefault (255 * value')
    store current $ iMax current' current''



sync :: Dopplers n -> Ivory (ProcEffects s t) ()
sync Dopplers {..} = do
    shouldSync <- mapM syncDoppler doppler
    let shouldTransmit = foldr (.||) false shouldSync
    when shouldTransmit $ T.lazyTransmit transport (1 + n) (\transmit -> do
            transmit actionDoppler1
            mapM_ transmit =<< mapM deref (current <$> doppler)
        )
    mapM_ ((`store` 0) . current) doppler



reset :: Dopplers n -> Ivory (ProcEffects s t) ()
reset Dopplers {..} = do
    mapM_ ((`store` 0) . count) doppler



syncDoppler :: Doppler a -> Ivory (ProcEffects s t) IBool
syncDoppler Doppler{..} = do
    current' <- deref current
    previous' <- deref previous
    let shouldSync = current' /=? previous'
    when shouldSync $ store previous current'
    pure shouldSync



average :: Num a => a -> a -> a -> a
average alpha a b = a * (1 - alpha) + b * alpha



iMax :: IvoryOrd a => a -> a -> a
iMax a b = (a >? b) ? (a, b)



alpha    = 0.0001
level    = 0.3
level'   = 0.05
low      = 3.6
high     = 3.8
maxCount = 16
