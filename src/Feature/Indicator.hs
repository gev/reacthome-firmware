{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

module Feature.Indicator where

import           Control.Monad.Reader    (MonadReader, asks)
import           Control.Monad.State     (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain             as D
import           Core.Handler
import           Core.Task
import qualified Core.Transport          as T
import           Data.Buffer
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Feature.RS485.RBUS.Data (RBUS (clock))
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Display       (Display, Render (Render))
import           Interface.Mac
import           Interface.MCU
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast



data Indicator n = forall d. Display d => Indicator
    { display   :: d
    , hue       :: IFloat
    , t         :: Value       Sint32
    , dt        :: Value       Sint32
    , phi       :: Value       Sint32
    , dphi      :: Value       Sint32
    , start     :: Value       IBool
    , findMe    :: Value       IBool
    , findMeMsg :: Buffer   2  Uint8
    , canvas    :: Canvas1D n
    , pixels    :: Records  n  RGB
    , transmit  :: forall   l. KnownNat l
                => Buffer   l  Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }


maxValue = 0.3 :: IFloat

indicator :: ( MonadState Context m
             , MonadReader (D.Domain p c) m
             , Display d, Handler (Render (Canvas1DSize n)) d
             , KnownNat n, KnownNat (Canvas1DSize n)
             , T.Transport t
             ) => (p -> m d) -> IFloat -> t -> m (Indicator n)
indicator mkDisplay hue transport = do
    mcu         <- asks D.mcu
    display     <- mkDisplay $ peripherals mcu
    frameBuffer <- values'     "indicator_frame_buffer" 0
    let canvas   = mkCanvas1D  frameBuffer
    t           <- value       "indicator_t"            0
    dt          <- value       "indicator_dt"           1
    phi         <- value       "indicator_phi"          0
    dphi        <- value       "indicator_dphi"         1
    start       <- value       "indicator_start"        true
    findMe      <- value       "indicator_find_me"      false
    findMeMsg   <- values      "indicator_find_me_msg"  [0xfa, 0]
    pixels      <- records_    "indicator_pixels"

    addStruct   (Proxy :: Proxy RGB)
    addStruct   (Proxy :: Proxy HSV)
    addConstArea sinT

    let indicator = Indicator { display, canvas, hue
                              , t, dt, phi, dphi
                              , start, findMe, findMeMsg
                              , pixels
                              , transmit = T.transmitBuffer transport
                              }

    addHandler $ Render display 25 frameBuffer $ do
        update indicator
        render indicator

    pure indicator



update :: KnownNat n => Indicator n -> Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    phi'   <- deref phi
    pixel  <- local . istruct $ hsv hue 1 maxValue
    start' <- deref start

    arrayMap $ \ix -> do
        let x = toIx (10 * fromIx ix + phi')
        sin' <- deref $ addrOf sinT ! x
        y    <- assign $ maxValue * (0.2 + 0.8 * sin')
        ifte_ start'
            (do
                let v' = safeCast phi' / 100
                store (pixel ~> s) v'
                store (pixel ~> v) $ y * v'
                when (phi' ==? 100) $ store start false
            )
            (   store (pixel ~> v) y
            )
        hsv'to'rgb pixel $ pixels ! ix

    findMe' <- deref findMe
    when findMe' $ do
        t' <- deref t
        store (pixel ~> v) 1
        store (pixel ~> s) 0.5
        hsv'to'rgb pixel $ pixels ! toIx ( 9 - t')
        hsv'to'rgb pixel $ pixels ! toIx (10 + t')
        cond_ [ t' ==? 0 ==> store dt   1
              , t' ==? 9 ==> store dt (-1)
              ]
        dt' <- deref dt
        store t  (t' + dt')
    cond_ [ phi' ==?   0 ==> store dphi   1
          , phi' ==? 200 ==> store dphi (-1)
          ]
    dphi' <- deref dphi
    store phi (phi' + dphi')



render :: (KnownNat n, KnownNat (Canvas1DSize n)) => Indicator n -> Ivory (ProcEffects s ()) ()
render Indicator{..} =
    writePixels canvas pixels


onFindMe :: KnownNat l => Indicator n -> Buffer l Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onFindMe Indicator{..} buff size =
    when (size >=? 2) $ do
        v <- unpack buff 1
        pack findMeMsg 1 v
        store findMe v
        transmit findMeMsg



sinT :: ConstMemArea (Array 200 (Stored IFloat))
sinT = constArea "indicator_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [-50..149]
    where f i = (1 + sin (pi * i / 100)) / 2
