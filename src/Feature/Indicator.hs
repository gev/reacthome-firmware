{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Indicator where

import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.Writer     (MonadWriter)
import           Core.Context
import           Core.Controller
import           Core.Domain              as D
import           Core.Feature
import qualified Interface.Display        as I
import           Interface.MCU

import           Core.Handler
import           Core.Task
import qualified Core.Transport           as T
import           Data.Buffer
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Display.FrameBuffer
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Feature.RS485.RBUS.Data  (RBUS (clock))
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Display        (Display (transmitFrameBuffer))
import           Interface.Mac
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast



data Indicator = forall o b. (I.Display o b, FrameBuffer b) => Indicator
    { display   :: o
    , canvas    :: Canvas1D 20 b
    , hue       :: IFloat
    , t         :: Value Sint32
    , dt        :: Value Sint32
    , t1        :: Value Sint32
    , dt1       :: Value Sint32
    , start     :: Value IBool
    , findMe    :: Value IBool
    , findMeMsg :: Buffer   2 Uint8
    , pixels    :: Records 20 RGB
    , transmit  :: forall n. KnownNat n
                => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


maxValue = 0.3 :: IFloat

indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , FrameBuffer b
             , I.Display o b
             , T.Transport t
             ) => (p -> m o) -> IFloat -> m Feature
indicator mkDisplay hue = do
    mcu       <- asks D.mcu
    transport <- asks D.transport
    display   <- mkDisplay $ peripherals mcu
    canvas    <- mkCanvas1D $ I.frameBuffer display "indicator"
    t         <- value    "indicator_t"           0
    dt        <- value    "indicator_dt"          1
    t1        <- value    "indicator_t1"          0
    dt1       <- value    "indicator_dt1"         1
    start     <- value    "indicator_start"       true
    findMe    <- value    "indicator_find_me"     false
    findMeMsg <- values   "indicator_find_me_msg" [0xfa, 0]
    pixels    <- records_ "indicator_pixels"

    addStruct   (Proxy :: Proxy RGB)
    addStruct   (Proxy :: Proxy HSV)
    addConstArea sinT

    let indicator = Indicator { display, canvas, hue
                              , t, dt, t1, dt1
                              , start, findMe, findMeMsg
                              , pixels
                              , transmit = T.transmitBuffer transport
                              }

    addHandler $ I.Render display 20 $ do
        update indicator
        render indicator

    pure $ Feature indicator



update :: Indicator -> Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    t1'    <- deref t1
    pixel  <- local . istruct $ hsv hue 1 maxValue
    start' <- deref start

    arrayMap $ \ix -> do
        let x = toIx (10 * fromIx ix + t1')
        sin' <- deref $ addrOf sinT ! x
        y    <- assign $ 0.1 + maxValue * sin'
        ifte_ start'
            (do
                let v' = safeCast t1' / 100
                store (pixel ~> s) v'
                store (pixel ~> v) $ y * v'
                when (t1' ==? 100) $ store start false
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
    cond_ [ t1' ==?   0 ==> store dt1   1
          , t1' ==? 200 ==> store dt1 (-1)
          ]
    dt1' <- deref dt1
    store t1 (t1' + dt1')



render :: Indicator -> Ivory (ProcEffects s ()) ()
render Indicator{..} = do
    writePixels canvas pixels
    transmitFrameBuffer display $ getBuffer canvas



instance Controller Indicator where
    handle Indicator{..} buff size = do
        action <- deref $ buff ! 0
        pure [ action ==? 0xfa ==>
                when (size >=? 2)
                     (do
                        v <- unpack buff 1
                        pack findMeMsg 1 v
                        store findMe v
                        transmit findMeMsg
                     )
             ]


sinT :: ConstMemArea (Array 200 (Stored IFloat))
sinT = constArea "sinT" $ iarray $ ival . ifloat . f . fromInteger <$> [-50..150]
    where f i = (1 + sin (pi * i / 100)) / 2
