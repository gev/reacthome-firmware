{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
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
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Display.FrameBuffer
import           Data.Record
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
    { display :: o
    , canvas  :: Canvas1D 20 b
    , color   :: [InitStruct HSV]
    , t1      :: Value IFloat
    , t       :: Value Sint32
    , dt      :: Value Sint32
    , pixels  :: Records 20 RGB
    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , FrameBuffer b
             , I.Display o b
             ) => (p -> m o) -> [InitStruct HSV] -> m Feature
indicator mkDisplay color = do
    mcu      <- asks D.mcu
    display  <- mkDisplay $ peripherals mcu
    canvas   <- mkCanvas1D $ I.frameBuffer display "indicator"
    t        <- value "indicator_t"  0
    t1       <- value "indicator_t1"  0
    dt       <- value "indicator_dt" 1
    pixels   <- records_ "indicator_pixels"

    addStruct (Proxy :: Proxy RGB)
    addStruct (Proxy :: Proxy HSV)

    let indicator = Indicator { display, canvas, color, t, t1, dt, pixels }

    addHandler $ I.Render display 10 $ do
        update indicator
        render indicator

    pure $ Feature indicator



update :: Indicator -> Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    t'    <- deref t
    t1'   <- deref t1
    pixel <- local $ istruct color
    v'    <- deref $ pixel ~> v

    arrayMap $ \ix -> do
        let x  = pi * (safeCast (fromIx ix) - 10 - t1') / 10
        let y  = 0.25 + 0.2 * cos x
        store (pixel ~> v) $ v' * y
        hsv'to'rgb pixel $ pixels ! ix

    store (pixel ~> v) 1
    hsv'to'rgb pixel $ pixels ! toIx ( 9 - t')
    hsv'to'rgb pixel $ pixels ! toIx (10 + t')

    cond_ [ t' ==? 0 ==> store dt   1
          , t' ==? 9 ==> store dt (-1)
          ]

    dt' <- deref dt
    store t  (t'  + dt' )
    store t1 (t1' + 0.01)



render :: Indicator -> Ivory (ProcEffects s ()) ()
render Indicator{..} = do
    writePixels canvas pixels
    transmitFrameBuffer display $ getBuffer canvas



instance Controller Indicator
