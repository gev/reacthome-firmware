{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.Touch where

import           Control.Monad                  (replicateM_)
import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Value
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Timer
import qualified Interface.Counter              as I
import qualified Interface.Touch                as I
import           Ivory.Language                 hiding (setBit)
import           Ivory.Stdlib                   (ifte, when)
import           Ivory.Stdlib.Control
import           Ivory.Support                  (symbol)
import           Support.CMSIS.CoreCMFunc       (disableIRQ, enableIRQ)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG
import Data.ByteString (index)
import Interface.Counter (Counter(readCounter))




data Touch = Touch { port          :: GPIO_PERIPH
                   , pin           :: GPIO_PIN
                   , timer         :: Timer
                   , thresholdLow  :: IFloat
                   , thresholdHigh :: IFloat
                   , timeAvg       :: Value IFloat
                --    , timeMin       :: Value IFloat
                   , time          :: Value IFloat
                   , delta         :: Value IFloat
                   , moments       :: Values 5 IFloat
                   , index         :: Value (Ix 5)
                   , sum           :: Value IFloat
                   , stateTouch    :: Value IBool
                   , debugVal      :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH -> IFloat -> IFloat -> m Touch
mkTouch port pin rcuPin thresholdLow thresholdHigh = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol port <> "_" <> symbol pin

    timeAvg    <- value   ("touch_time_avg" <> name) 0
    time       <- value   ("touch_time" <> name) 0
    delta      <- value   ("touch_delta" <> name) 0
    moments    <- values' ("touch_moments" <> name) 0
    index      <- value   ("touch_index" <> name) 0
    sum        <- value   ("touch_sum" <> name) 0
    stateTouch <- value   ("touch_state_touch" <> name) false
    debugVal   <- value   ("debug_val" <> name) 0

    addInit name $ do
        enablePeriphClock rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

    let touch = Touch { port
                      , pin
                      , timer
                      , thresholdLow
                      , thresholdHigh
                      , timeAvg
                      , time
                      , delta
                      , moments
                      , index
                      , sum
                      , stateTouch
                      , debugVal
                      }

    -- addTask $ delay 5_000 ("reset_avg" <> name) $ resetAvg touch
    -- addTask $ delayPhase 5 0 ("touch_reset" <> name) $ reset touch
    addTask $ delay 5 ("touch_run" <> name) $ runMeasurement touch

    pure touch

resetAvg :: Touch -> Ivory eff ()
resetAvg Touch{..} = do
    state <- deref stateTouch
    when (iNot state) $
        store timeAvg 0xffff

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    run = runMeasurement
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch


reset :: Touch -> Ivory (ProcEffects s ()) ()
reset t@Touch{..} = do
    modePort port pin gpio_mode_output
    resetBit port pin

wait :: Touch -> Uint16 -> Ivory (ProcEffects s ()) ()
wait Touch{..} t = do
    t1 <- readCounter timer
    forever $ do
        t2 <- readCounter timer
        when (t2 - t1 >? t) breakOut

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement t@Touch{..} = do
    -- ix <- deref index
    -- store index $ ix + 1

    modePort port pin gpio_mode_output
    resetBit port pin
    disableIRQ
    -- wait t 16_800
    modePort port pin gpio_mode_input
    t1 <- I.readCounter timer
    forever $ do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut
    t2 <- I.readCounter timer
    enableIRQ
    -- modePort port pin gpio_mode_output
    -- resetBit port pin
    -- forever $ do
    --     t3 <- I.readCounter timer
    --     when (t3 - t2 >? 420) breakOut
    let next = safeCast $ t2 - t1
    prev <- deref time
    store time next

    store debugVal next

    -- let moment = moments ! ix
    -- last <- deref moment
    -- s' <- deref sum
    -- store sum $ s' + next - last
    -- store moment next

    -- s' <- deref sum
    -- let m = s' / arrayLen moments

    -- min <- local $ ival 0xffff
    -- max <- local $ ival 0
    -- arrayMap $ \ix -> do
    --     t' <- deref $ moments ! ix
    --     min' <- deref min
    --     when (t' <? min') $ store min t'
    --     max' <- deref max
    --     when (t' >? max') $ store max t'

    -- min'' <- deref min
    -- max'' <- deref max
    -- n     <- local $ ival (0 :: Uint16)
    -- s     <- local izero
    -- let d = max'' - min''
    -- let min''' = min'' + 0 * d
    -- let max''' = max'' - 0 * d
    -- arrayMap $ \ix -> do
    --     t' <- deref $ moments ! ix
    --     when (d <? 100 .|| t' >? min''' .&& t' <? max''') $ do
    --         n' <- deref n
    --         store n $ n' + 1
    --         s' <- deref s
    --         store s $ s' + t'

    -- n'' <- safeCast <$> deref n
    -- when (n'' >? 10) $ do
    --     s'' <- deref s
    --     let m = s'' / n''
    --     dt <- local $ ival d
    --     arrayMap $ \ix -> do
    --         t' <- deref $ moments ! ix
    --         when (d <? 100 .|| t' >? min''' .&& t' <? max''') $ do
    --             dt' <- deref dt
    --             store dt $ dt' + abs (t' - m)

    --     dt'' <- (/ n'') <$> deref dt

    -- store debugVal next


        -- time' <- deref time
        -- store time $ average 0.02 time' t'

        -- avg' <- deref timeAvg
        -- store timeAvg $ average 0.002 avg' t'

        -- time'' <- deref time
        -- avg'' <- deref timeAvg
        -- let t = time'' - avg''
        -- -- state <- deref stateTouch
        -- cond_ [ t >? thresholdHigh ==> do
        --             store stateTouch true
        --             -- when (iNot state) $ store debugVal t
        --       , t <? thresholdLow  ==> do
        --             store stateTouch false
        --       ]


        -- delta' <- deref delta
        -- cond_ [ t >? thresholdHigh ==> store delta (average 0.02 delta' 1)
        --       , t <? thresholdLow  ==> store delta (average 0.02 delta' 0)
        --       ]

        -- delta'' <- deref delta
        -- cond_ [ delta'' >? 0.8 ==> store stateTouch true
        --       , delta'' <? 0.2 ==> store stateTouch false
        --       ]






average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
