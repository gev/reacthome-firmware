{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Indicator where

import           Control.Monad.Reader
import           Control.Monad.Writer
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
import           Data.Value
import           Feature.RS485.RBUS.Data  (RBUS (clock))
import           GHC.TypeNats
import           Interface.Counter
import           Interface.Display        (Display (transmitFrameBuffer))
import           Interface.Mac
import           Ivory.Language
import           Ivory.Stdlib



data Indicator = forall o b. (I.Display o b, FrameBuffer b) => Indicator
    { display :: o
    , canvas  :: Canvas1D 20 b
    , color   :: RGB
    , t       :: Value Sint32
    , dt      :: Value Sint32
    }


indicator :: ( MonadWriter Context m
             , MonadReader (D.Domain p t) m
             , FrameBuffer b
             , I.Display o b
             ) => (p -> m o) -> RGB -> m Feature
indicator mkDisplay color = do
    mcu      <- asks D.mcu
    display  <- mkDisplay $ peripherals mcu
    canvas   <- mkCanvas1D $ I.frameBuffer display "indicator"
    t        <- value "indicator_t"  0
    dt       <- value "indicator_dt" 1

    let indicator = Indicator { display, canvas, color, t, dt }

    addHandler $ I.Render display 10 $ do
        update indicator
        render indicator

    pure $ Feature indicator



update :: Indicator -> Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    t'  <- deref t
    cond_ [ t' ==? 0 ==> store dt   1
          , t' ==? 9 ==> store dt (-1)
          ]
    dt' <- deref dt
    store t (t' + dt')



render :: Indicator -> Ivory (ProcEffects s ()) ()
render Indicator{..} = do
    t' <- deref t
    clearCanvas canvas
    writePixel canvas (toIx $  9 - t') color
    writePixel canvas (toIx $ 10 + t') color
    transmitFrameBuffer display $ getBuffer canvas



instance Controller Indicator
