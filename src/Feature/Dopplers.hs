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
import           Ivory.Language.Float (IFloat (IFloat))
import           Ivory.Language.Uint  (Uint8 (Uint8))
import           Ivory.Stdlib



data Doppler a = Doppler
    { adc         :: a
    , expectation :: Value IFloat
    , measurement :: Value IFloat
    , threshold   :: Value IFloat
    , current     :: Value Uint8
    , previous    :: Value Uint8
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

    addTask $ delay   1 "doppler_measure" $ mapM_ measure doppler
    addTask $ delay 200 "doppler_sync"    $ sync dopplers

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
    measurement      <- value (name <> "measurement")   0
    expectation      <- value (name <> "expectation") 0.5
    threshold        <- value (name <> "threshold"  )   1
    current          <- value (name <> "current"    )   0
    previous         <- value (name <> "previous"   )   0

    let doppler = Doppler { adc
                          , measurement
                          , expectation
                          , threshold
                          , current
                          , previous
                          }

    pure doppler



measure :: I.ADC a => Doppler a -> Ivory (ProcEffects s ()) ()
measure Doppler {..} = do
    a <- I.getReduced adc

    expectation' <- deref expectation
    store expectation $ average long expectation' a

    let diff = abs $ a - expectation'

    measurement' <- deref measurement
    let measurement'' = average short measurement' diff
    store measurement measurement''

    threshold' <- deref threshold
    when (measurement'' <? low * threshold')
         (store threshold $ average long threshold' measurement'')

    let range = iMax expectation' $ 1 - expectation'

    let threshold'' = high * threshold'
    let current'' = (measurement'' >? threshold'')
          ? (castDefault $ 255 * sqrt (measurement'' - threshold'') / (range - threshold''), 0)

    current' <- deref current
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



short = 0.02
long  = 0.0002
low   = 2.2
high  = 2.8
