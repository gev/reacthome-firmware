{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Interface.OneWire
    ( OneWire
    , mkOneWire
    , reset
    , write
    , read
    , skipROM
    , selectROM
    ) where

import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.FSM
import           Core.Handler
import           Core.Task                (yeld)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           GHC.TypeNats
import qualified Interface.GPIO.OpenDrain as OD
import qualified Interface.Timer          as T
import           Ivory.Language
import           Ivory.Stdlib
import           Prelude                  hiding (read)






data OneWire where
  OneWire :: (OD.OpenDrain od, T.Timer t)
          => { port     :: od
             , timer    :: t
             , state    :: Value Uint8
             , time     :: Value Uint8
             , stateB   :: Buffer 32 Uint8
             , stateQ   :: Queue 32
             , tmpB     :: Buffer 32 Uint8
             , tmpQ     :: Queue 32
             , tmpV     :: Value Uint8
             , width    :: Value Uint8
             , count    :: Value Uint8
             } -> OneWire


stateWrite          = 0x0 :: Uint8
stateRead           = 0x1 :: Uint8
stateReset          = 0x2 :: Uint8
stateWaitPresence   = 0x3 :: Uint8
stateWaitReady      = 0x4 :: Uint8
stateReady          = 0x5 :: Uint8
stateResult         = 0x6 :: Uint8
stateError          = 0x7 :: Uint8


timeReset           =  48 :: Uint8
timeWaitPresence    =  55 :: Uint8
timeWaitReady       =  97 :: Uint8
timeWrite0          =   7 :: Uint8
timeWrite1          =   1 :: Uint8
timeWriteSlot       =   8 :: Uint8
timeReadPrepare     =   1 :: Uint8
timeWaitBit         =   2 :: Uint8
timeReadSlot        =   8 :: Uint8


mkOneWire :: (MonadState Context m, OD.OpenDrain od, T.Timer t)
          => (Uint32 -> Uint32 -> m t)
          -> m od
          -> m OneWire
mkOneWire cfg od = do
    port   <- od
    timer  <- cfg    1_000_000 10
    state  <- value  "one_wire_state" stateReady
    time   <- value_ "one_wire_time"
    stateB <- buffer "one_wire_state"
    stateQ <- queue  "one_wire_state"
    tmpB   <- buffer "one_wire_tmp"
    tmpQ   <- queue  "one_wire_tmp"
    tmpV   <- value_ "one_wire_tmp_value"
    width  <- value_ "one_wire_bit_width"
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
                          , width
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
    [ stateReady  |-> handleReady
    , stateResult |-> handleResult
    , stateError  |-> handleError
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


handleReady :: OneWire -> Ivory eff ()
handleReady ow@OneWire {..} = popState ow $ \nextState ->
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


handleResult :: OneWire -> Ivory eff ()
handleResult OneWire{..} = store state stateReady


handleError :: OneWire -> Ivory eff ()
handleError ow@OneWire {..} = popState ow $ \nextState ->
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
            (store state stateReady)
            (store state stateError)
    , true ==> store time (time' + 1)
    ]


doWrite :: OneWire -> Uint8 -> Ivory eff ()
doWrite OneWire{..} time' = deref width >>= \width' -> cond_
    [ time' ==? 0 ==> do
        count' <- deref count
        ifte_ (count' <? 8)
              (do
                    OD.reset port
                    tmpV' <- deref tmpV
                    let bit = (tmpV' `iShiftR` count') .& 1
                    ifte_ (bit ==? 1)
                        (store width timeWrite1)
                        (store width timeWrite0)
                    store count $ count' + 1
                    store time 1
              )
              (store state stateReady)
    , time' ==? width' ==> OD.set port >> store time (time' + 1)
    , time' ==? timeWriteSlot ==> store time 0
    , true  ==> store time (time' + 1)
    ]


doRead :: OneWire -> Uint8 -> Ivory eff ()
doRead OneWire{..} time' = deref width >>= \width' -> cond_
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
              (store state stateResult)
              (store time 0)
    , true  ==> store time (time' + 1)
    ]



reset :: OneWire -> Ivory eff ()
reset ow = pushState ow stateReset

read :: OneWire -> Ivory eff ()
read ow = pushState ow stateRead

write :: OneWire -> Uint8 -> Ivory eff ()
write ow v = pushTmp ow v
          >> pushState ow stateWrite

skipROM :: OneWire -> Ivory eff ()
skipROM ow = write ow 0xcc

selectROM :: OneWire -> Ivory eff ()
selectROM ow = write ow 0x55






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
