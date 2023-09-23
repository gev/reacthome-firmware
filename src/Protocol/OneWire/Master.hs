{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Protocol.OneWire.Master
    ( OneWireMaster
    , mkOneWireMaster
    , reset
    , write
    , read
    , skipROM
    , selectROM
    ) where

import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.FSM
import           Core.Handler
import           Core.Task             (yeld)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           GHC.TypeNats
import           Interface.OneWire
import           Ivory.Language
import           Ivory.Stdlib
import           Prelude               hiding (read)



data OneWireMaster = OneWireMaster
    { onewire :: OneWire
    , state   :: Value       Uint8
    , time    :: Value       Uint8
    , stateB  :: Buffer  32  Uint8
    , stateQ  :: Queue   32
    , tmpB    :: Buffer  32  Uint8
    , tmpQ    :: Queue   32
    , tmpV    :: Value       Uint8
    , width   :: Value       Uint8
    , count   :: Value       Uint8
    , onData  :: forall eff. Uint8 -> Ivory eff ()
    , onError :: forall eff. Uint8 -> Ivory eff ()
    }


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


mkOneWireMaster :: MonadState Context m
                => m OneWire
                -> (forall eff. Uint8 -> Ivory eff ())
                -> (forall eff. Uint8 -> Ivory eff ())
                -> m OneWireMaster
mkOneWireMaster ow onData onError = do
    onewire <- ow
    state   <- value  "one_wire_state" stateReady
    time    <- value_ "one_wire_time"
    stateB  <- buffer "one_wire_state"
    stateQ  <- queue  "one_wire_state"
    tmpB    <- buffer "one_wire_tmp"
    tmpQ    <- queue  "one_wire_tmp"
    tmpV    <- value_ "one_wire_tmp_value"
    width   <- value_ "one_wire_bit_width"
    count   <- value_ "one_wire_count"

    let master  = OneWireMaster { onewire
                                , state
                                , time
                                , stateB
                                , stateQ
                                , tmpB
                                , tmpQ
                                , tmpV
                                , width
                                , count
                                , onData
                                , onError
                                }

    handleTimer onewire $ handleOneWire master
    addTask $ yeld "one_wire" $ taskOneWire master

    pure master



taskOneWire :: OneWireMaster -> Ivory eff ()
taskOneWire = runState' state
    [ stateReady  |-> handleReady
    , stateResult |-> handleResult
    , stateError  |-> handleError
    ]


handleOneWire :: OneWireMaster -> Ivory eff ()
handleOneWire ow = runState state
    [ stateReset        |-> doReset
    , stateWaitPresence |-> waitPresence
    , stateWaitReady    |-> waitReady
    , stateWrite        |-> doWrite
    , stateRead         |-> doRead
    ] ow =<< deref (time ow)



{-
-   TODO: Should we start/stop timer?
-}


handleReady :: OneWireMaster -> Ivory eff ()
handleReady m@OneWireMaster {..} = popState m $ \nextState ->
    cond_ [ nextState ==? stateWrite ==> do
                popTmp m $ \v -> do
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


handleResult :: OneWireMaster -> Ivory eff ()
handleResult OneWireMaster{..} = do
    onData =<< deref tmpV
    store state stateReady

handleError m@OneWireMaster {..} = popState m $ \nextState ->
    when (nextState ==? stateReset) $ do
        store time  0
        store state stateReset



doReset :: OneWireMaster -> Uint8 -> Ivory eff ()
doReset OneWireMaster{..} time' = cond_
    [ time' ==? 0 ==> do
        pullDown onewire
        store time 1
    , time' ==? timeReset ==> do
        pullUp onewire
        store state stateWaitPresence
    , true ==> store time (time' + 1)
    ]


waitPresence :: OneWireMaster -> Uint8 -> Ivory eff ()
waitPresence OneWireMaster{..} time' = cond_
    [ time' ==? timeWaitPresence ==> do
        hasPresence <- iNot <$> getState onewire
        ifte_ hasPresence
            (store state stateWaitReady)
            (store state stateError)
    , true ==> store time (time' + 1)
    ]


waitReady :: OneWireMaster -> Uint8 -> Ivory eff ()
waitReady OneWireMaster{..} time' = cond_
    [ time' ==? timeWaitReady ==> do
        hasntPresence <- getState onewire
        ifte_ hasntPresence
            (store state stateReady)
            (store state stateError)
    , true ==> store time (time' + 1)
    ]


doWrite :: OneWireMaster -> Uint8 -> Ivory eff ()
doWrite OneWireMaster{..} time' = deref width >>= \width' -> cond_
    [ time' ==? 0 ==> do
        count' <- deref count
        ifte_ (count' <? 8)
              (do
                    pullDown onewire
                    tmpV' <- deref tmpV
                    let bit = (tmpV' `iShiftR` count') .& 1
                    ifte_ (bit ==? 1)
                        (store width timeWrite1)
                        (store width timeWrite0)
                    store count $ count' + 1
                    store time 1
              )
              (store state stateReady)
    , time' ==? width' ==> pullUp onewire >> store time (time' + 1)
    , time' ==? timeWriteSlot ==> store time 0
    , true  ==> store time (time' + 1)
    ]


doRead :: OneWireMaster -> Uint8 -> Ivory eff ()
doRead OneWireMaster{..} time' = deref width >>= \width' -> cond_
    [ time' ==? 0 ==> do
        pullDown onewire
        store tmpV 0
        store time 1
    , time' ==? timeReadPrepare ==> do
        pullUp onewire
        store time (time' + 1)
    , time' ==? timeWaitBit ==> do
        count' <- deref count
        tmpV'  <- deref tmpV
        bit <- getState onewire
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



reset :: OneWireMaster -> Ivory eff ()
reset m = pushState m stateReset

read :: OneWireMaster -> Ivory eff ()
read m = pushState m stateRead

write :: OneWireMaster -> Uint8 -> Ivory eff ()
write m v = pushTmp m v
          >> pushState m stateWrite

skipROM :: OneWireMaster -> Ivory eff ()
skipROM m = write m 0xcc

selectROM :: OneWireMaster -> Ivory eff ()
selectROM m = write m 0x55





pop' :: KnownNat n => Queue n -> Buffer n Uint8 -> (Uint8 -> Ivory eff ())-> Ivory eff ()
pop' q b run = pop q $ \i -> run =<< deref (b ! toIx i)

popState :: OneWireMaster -> (Uint8 -> Ivory eff ()) -> Ivory eff ()
popState OneWireMaster{..} = pop' stateQ stateB

popTmp :: OneWireMaster -> (Uint8 -> Ivory eff ()) -> Ivory eff ()
popTmp OneWireMaster{..} = pop' tmpQ tmpB


push' :: KnownNat n => Queue n -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
push' q b v = push q $ \i -> store (b ! toIx i) v

pushState :: OneWireMaster -> Uint8 -> Ivory eff ()
pushState OneWireMaster{..} = push' stateQ stateB

pushTmp :: OneWireMaster -> Uint8 -> Ivory eff ()
pushTmp OneWireMaster{..} = push' tmpQ tmpB
