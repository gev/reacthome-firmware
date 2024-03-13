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



import           Control.Monad        (zipWithM)
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.State  (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain          as D
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
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
    }



data Dopplers = forall a t. (I.ADC a, T.LazyTransport t) => Dopplers
    { n         :: Int
    , doppler   :: [Doppler a]
    , transport :: t
    }



dopplers :: ( MonadState Context m
            , MonadReader (D.Domain p t c) m
            , T.LazyTransport t
            , I.ADC a
            ) => [p -> m a] -> m Dopplers
dopplers analogInput = do
    mcu              <- asks D.mcu
    transport        <- asks D.transport
    doppler          <- zipWithM mkDoppler [1..] analogInput

    let dopplers = Dopplers { n = length analogInput
                            , doppler
                            , transport
                            }

    addTask $ delay   1 "doppler_measure" $ mapM_ measure doppler
    addTask $ delay 200 "doppler_sync"    $ sync dopplers

    pure dopplers



mkDoppler :: ( MonadState Context m
             , MonadReader (D.Domain p timeout c) m
             , T.LazyTransport timeout
             , I.ADC a
             ) => Int -> (p -> m a) -> m (Doppler a)
mkDoppler index analogInput = do
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
    when (measurement'' <? 3 * threshold')
         (store threshold $ average long threshold' measurement'')

    let range = iMax expectation' $ 1 - expectation'

    let threshold'' = 5 * threshold'
    let current'' = (measurement'' >? threshold'')
          ? (castDefault $ 255 * sqrt (measurement'' - threshold'') / (range - threshold''), 0)

    current' <- deref current
    store current $ iMax current' current''



sync :: Dopplers -> Ivory (ProcEffects s t) ()
sync Dopplers {..} = do
    shouldSync <- mapM syncDoppler doppler
    let shouldTransmit = foldr (.||) false shouldSync
    when shouldTransmit $ T.lazyTransmit transport (1 + fromIntegral n) (\transmit -> do
            transmit actionDoppler
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



short   = 0.05     :: IFloat
long    = 0.0005   :: IFloat
