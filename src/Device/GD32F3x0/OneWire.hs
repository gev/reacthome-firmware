{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.OneWire where

import           Control.Monad.State            (MonadState)
import           Core.Context
import           Core.FSM
import           Core.Handler
import           Core.Task                      (yeld)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.OpenDrain
import           Device.GD32F3x0.GPIO.Port
import           Device.GD32F3x0.Timer          (Timer)
import           GHC.Float                      (timesFloat)
import           GHC.TypeNats
import qualified Interface.GPIO.OpenDrain       as OD
import qualified Interface.OneWire              as OW
import qualified Interface.Timer                as T
import           Ivory.Language
import           Ivory.Stdlib



data OneWire = OneWire
    { port   :: OpenDrain
    , timer  :: Timer
    , state  :: Value     Uint8
    , time   :: Value     Uint8
    , stateB :: Buffer 32 Uint8
    , stateQ :: Queue  32
    , tmpB   :: Buffer 32 Uint8
    , tmpQ   :: Queue  32
    , tmpV   :: Value     Uint8
    , delay  :: Value     Uint8
    , count  :: Value     Uint8
    }


stateWrite          = 0x0 :: Uint8
stateRead           = 0x1 :: Uint8
stateReset          = 0x2 :: Uint8
stateWaitPresence   = 0x3 :: Uint8
stateWaitReady      = 0x4 :: Uint8
stateDone           = 0x5 :: Uint8
stateError          = 0x6 :: Uint8


timeReset           =  48 :: Uint8
timeWaitPresence    =  55 :: Uint8
timeWaitReady       =  97 :: Uint8
timeWrite0          =   7 :: Uint8
timeWrite1          =   1 :: Uint8
timeWriteSlot       =   8 :: Uint8
timeReadPrepare     =   1 :: Uint8
timeWaitBit         =   2 :: Uint8
timeReadSlot        =   8 :: Uint8


mkOneWire :: MonadState Context m
         => (Uint32 -> Uint32 -> m Timer)
         -> m OpenDrain
         -> m OneWire
mkOneWire cfg od = do
    port   <- od
    timer  <- cfg    1_000_000 10
    state  <- value  "one_wire_state" stateDone
    time   <- value_ "one_wire_time"
    stateB <- buffer "one_wire_state"
    stateQ <- queue  "one_wire_state"
    tmpB   <- buffer "one_wire_tmp"
    tmpQ   <- queue  "one_wire_tmp"
    tmpV   <- value_ "one_wire_tmp_value"
    delay  <- value_ "one_wire_delay"
    count  <- value_ "one_wire_count"

    let onewire = OneWire { port
                          , timer
                          , state
                          , time
                          , stateB
                          , stateQ
                          , tmpB
                          , tmpQ
                          , tmpV
                          , delay
                          , count
                          }

    addInit "onewire" $ initOneWire onewire
    addHandler $ T.HandleTimer timer $ handlerOneWire onewire
    addTask $ yeld "one_wire" $ taskOneWire onewire

    pure onewire



initOneWire :: OneWire -> Ivory eff ()
initOneWire OneWire {..} = OD.set port


taskOneWire :: OneWire -> Ivory eff ()
taskOneWire = runState' state
    [ stateDone  |-> onDone
    , stateError |-> onError
    ]


handlerOneWire :: OneWire -> Ivory eff ()
handlerOneWire ow = runState state
    [ stateReset        |-> doReset
    , stateWaitPresence |-> waitPresence
    , stateWaitReady    |-> waitReady
    , stateWrite        |-> doWrite
    , stateRead         |-> doRead
    ] ow =<< deref (time ow)



{-
-   TODO: Should we start/stop timer?
-}


onDone :: OneWire -> Ivory eff ()
onDone ow@OneWire {..} = popState ow $ \nextState ->
    cond_ [ nextState ==? stateWrite ==> do
                popTmp ow $ \v -> do
                    store tmpV  v
                    store count 0
                    store time  0
                    store state nextState
          , nextState ==? stateRead ==> do
                store count 0
                store time  0
                store state nextState
          , true ==> do
                store time  0
                store state nextState
          ]


onError :: OneWire -> Ivory eff ()
onError ow@OneWire {..} = popState ow $ \nextState ->
    when (nextState ==? stateReset) $ do
        store time  0
        store state stateReset



doReset :: OneWire -> Uint8 -> Ivory eff ()
doReset OneWire{..} time' = cond_
    [ time' ==? 0 ==> do
        OD.reset port
        store time 1
    , time' ==? timeReset ==> do
        OD.set port
        store state stateWaitPresence
    , true ==> store time (time' + 1)
    ]


waitPresence :: OneWire -> Uint8 -> Ivory eff ()
waitPresence OneWire{..} time' = cond_
    [ time' ==? timeWaitPresence ==> do
        hasPresence <- iNot <$> OD.get port
        ifte_ hasPresence
            (store state stateWaitReady)
            (store state stateError)
    , true ==> store time (time' + 1)
    ]


waitReady :: OneWire -> Uint8 -> Ivory eff ()
waitReady OneWire{..} time' = cond_
    [ time' ==? timeWaitReady ==> do
        hasntPresence <- OD.get port
        ifte_ hasntPresence
            (store state stateDone)
            (store state stateError)
    , true ==> store time (time' + 1)
    ]


doWrite :: OneWire -> Uint8 -> Ivory eff ()
doWrite OneWire{..} time' = deref delay >>= \delay' -> cond_
    [ time' ==? 0 ==> do
        count' <- deref count
        ifte_ (count' <? 8)
              (do
                    OD.reset port
                    tmpV' <- deref tmpV
                    let bit = (tmpV' `iShiftR` count') .& 1
                    ifte_ (bit ==? 1)
                        (store delay timeWrite1)
                        (store delay timeWrite0)
                    store count $ count' + 1
                    store time 1
              )
              (store state stateDone)
    , time' ==? delay' ==> OD.set port >> store time (time' + 1)
    , time' ==? timeWriteSlot ==> store time 0
    , true  ==> store time (time' + 1)
    ]


doRead :: OneWire -> Uint8 -> Ivory eff ()
doRead OneWire{..} time' = deref delay >>= \delay' -> cond_
    [ time' ==? 0 ==> do
        OD.reset port
        store tmpV 0
        store time 1
    , time' ==? timeReadPrepare ==> do
        OD.set port
        store time (time' + 1)
    , time' ==? timeWaitBit ==> do
        count' <- deref count
        tmpV'  <- deref tmpV
        bit <- OD.get port
        store tmpV $ tmpV' .| (safeCast bit `iShiftL` count')
        store count $ count' + 1
        store time $ time' + 1
    , time' ==? timeReadSlot ==> do
        count' <- deref count
        ifte_ (count' ==? 8)
              (store state stateDone)
              (store time 0)
    , true  ==> store time (time' + 1)
    ]


instance OW.OneWire OneWire where

    reset ow    = pushState ow stateReset

    read  ow    = pushState ow stateRead

    write ow v  = pushTmp   ow v
               >> pushState ow stateWrite



instance Handler OW.HandleOneWire OneWire where
  addHandler _ = pure ()



pop' :: KnownNat n => Queue n -> Buffer n Uint8 -> (Uint8 -> Ivory eff ())-> Ivory eff ()
pop' q b run = pop q $ \i -> run =<< deref (b ! toIx i)

popState :: OneWire -> (Uint8 -> Ivory eff ()) -> Ivory eff ()
popState OneWire{..} = pop' stateQ stateB

popTmp :: OneWire -> (Uint8 -> Ivory eff ()) -> Ivory eff ()
popTmp OneWire{..} = pop' tmpQ tmpB


push' :: KnownNat n => Queue n -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
push' q b v = push q $ \i -> store (b ! toIx i) v

pushState :: OneWire -> Uint8 -> Ivory eff ()
pushState OneWire{..} = push' stateQ stateB

pushTmp :: OneWire -> Uint8 -> Ivory eff ()
pushTmp OneWire{..} = push' tmpQ tmpB
