module Feature.Dopplers (
    Dopplers,
    dopplers,
) where

import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Task
import qualified Core.Transport as T
import Data.Buffer
import Data.Data (cast)
import Data.Fixed
import Data.Value
import qualified Interface.ADC as I
import Interface.MCU
import Ivory.Language
import Ivory.Language.Float (IFloat (IFloat))
import Ivory.Stdlib

data Doppler a = Doppler
    { adc :: a
    , expectation :: Value IFloat
    , current :: Value Uint8
    , previous :: Value Uint8
    , median0 :: Value IFloat
    , median1 :: Value IFloat
    , count :: Value (Ix N)
    }

data Dopplers n = forall a t. (I.ADC a, T.LazyTransport t) => Dopplers
    { n :: Uint8
    , doppler :: List n (Doppler a)
    , transport :: t
    }

dopplers ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.LazyTransport t
    , I.ADC a
    ) =>
    List n (p -> m a) ->
    t ->
    m (Dopplers n)
dopplers analogInput transport = do
    mcu <- asks D.mcu
    doppler <- zipWithM (mkDoppler transport) analogInput nats

    let dopplers =
            Dopplers
                { n = fromIntegral $ length analogInput
                , doppler
                , transport
                }

    addTask $ delay 1 "doppler_measure" $ mapM_ measure doppler
    addTask $ delay t "doppler_sync" $ sync dopplers

    pure dopplers

mkDoppler ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.LazyTransport t
    , I.ADC a
    ) =>
    t ->
    (p -> m a) ->
    Int ->
    m (Doppler a)
mkDoppler transport analogInput index = do
    let name = "doppler_" <> show index <> "_"
    mcu <- asks D.mcu
    adc <- analogInput $ peripherals mcu
    expectation <- value (name <> "expectation") 0.5
    current <- value (name <> "current") 0
    previous <- value (name <> "previous") 0
    median0 <- value (name <> "median0") 0
    median1 <- value (name <> "median1") $ level / 2
    count <- value (name <> "count") 0

    let doppler =
            Doppler
                { adc
                , expectation
                , current
                , previous
                , median0
                , median1
                , count
                }

    pure doppler

measure :: (I.ADC a) => Doppler a -> Ivory (ProcEffects s ()) ()
measure Doppler{..} = do
    a' <- I.getReduced adc

    expectation' <- deref expectation
    let diff = abs $ a' - expectation'

    when (diff <? range) $
        store expectation $
            average alpha expectation' a'

    median0' <- deref median0
    median1' <- deref median1
    measurement <- local $ ival 0

    cond_
        [ diff
            >? level
            ==> store measurement diff
            >> store median0 (average (betta * level) median0' level)
        , diff
            >? median1'
            ==> store median0 (average (betta * diff) median0' diff)
        , true
            ==> store median0 (average (betta * median1') median0' median1')
        ]

    let threshold = average gamma median1' level
    when (diff <? threshold) $
        store median1 (average alpha median1' diff)

    let threshold' = average gamma' median1' level
    measurement' <- deref measurement
    let value' = iMax measurement' median0' - threshold'
    let maxRange = iMax expectation' (1 - expectation') - threshold'
    let x = value' / maxRange
    let current'' = castDefault $ 127.5 * (x + sqrt x)
    current' <- deref current
    store current $ iMax current' current''

sync :: Dopplers n -> Ivory (ProcEffects s t) ()
sync Dopplers{..} = do
    shouldSync <- mapM shouldSyncDoppler doppler
    let shouldTransmit = foldr (.||) false shouldSync
    when shouldTransmit $ T.lazyTransmit transport (1 + n) \transmit -> do
        transmit actionDoppler1
        mapM_ transmit =<< mapM deref (current <$> doppler)
    mapM_
        ( \Doppler{..} -> do
            count' <- deref count
            store count $ count' + 1
            when (count' ==? 0) $ store current 0
        )
        doppler

shouldSyncDoppler :: Doppler a -> Ivory (ProcEffects s t) IBool
shouldSyncDoppler Doppler{..} = do
    count' <- deref count
    current' <- deref current
    previous' <- deref previous
    let shouldSync = current' >? previous' .|| count' ==? 0
    when shouldSync $ store previous current'
    pure shouldSync

average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = a * (1 - alpha) + b * alpha

iMax :: (IvoryOrd a) => a -> a -> a
iMax a b = (a >? b) ? (a, b)

sub = (-)

type N = 5

t = 200
range = 0.3
level = 0.1
alpha = 0.0001
betta = 0.05
gamma = 0.15
gamma' = 0.12
