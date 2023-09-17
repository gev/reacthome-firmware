{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.OneWire where

import           Control.Monad.State            (MonadState)
import           Core.Context
import           Core.Handler
import           Core.Task                      (yeld)
import           Data.Bits                      (Bits (bit))
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
import           Support.CMSIS.CoreCM4



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
taskOneWire OneWire {..} = do
    currentState <- deref state
    cond_ [ currentState ==? stateError ==> do
                pop stateQ $ \i -> do
                    nextState <- deref (stateB ! toIx i)
                    when (nextState ==? stateReset) $ do
                        store time  0
                        store state nextState
          , currentState ==? stateDone ==> do
                pop stateQ $ \i -> do
                    nextState <- deref (stateB ! toIx i)
                    ifte_ (nextState ==? stateWrite)
                          (pop tmpQ $ \j -> do
                                store tmpV =<< deref (tmpB ! toIx j)
                                store count 0
                                store time  0
                                store state nextState
                          )
                          (do
                                store time  0
                                store state nextState
                          )
          ]



{-
-   TODO: Should we start/stop timer?
-}
handlerOneWire :: OneWire -> Ivory eff ()
handlerOneWire OneWire {..} = do
    state' <- deref state
    time'  <- deref time
    store time (time' + 1)
    cond_ [ state' ==? stateReset ==>
                cond_ [ time' ==? 0 ==> do
                            OD.reset port
                      , time' ==? timeReset ==> do
                            OD.set port
                            store state stateWaitPresence
                      ]
          , state' ==? stateWaitPresence .&& time' ==? timeWaitPresence ==> do
                hasPresence <- iNot <$> OD.get port
                ifte_ hasPresence
                    (store state stateWaitReady)
                    (store state stateError)
          , state' ==? stateWaitReady .&& time' ==? timeWaitReady ==> do
                hasntPresence <- OD.get port
                ifte_ hasntPresence
                    (store state stateDone)
                    (store state stateError)
          , state' ==? stateWrite ==> do
                delay' <- deref delay
                cond_ [ time' ==? 0 ==> do
                            count' <- deref count
                            ifte_ (count' <? 8)
                                  (do
                                        OD.reset port
                                        tmpV'  <- deref tmpV
                                        let bit = (tmpV' `iShiftR` count') .& 1
                                        ifte_ (bit ==? 1)
                                            (store delay timeWrite1)
                                            (store delay timeWrite0)
                                        store count $ count' + 1
                                  )
                                  (store state stateDone)
                      , time' ==? delay' ==> do
                            OD.set port
                      , time' ==? timeWriteSlot ==> do
                            store time 0
                      ]
          ]


instance OW.OneWire OneWire where

    reset ow    = do pushState ow stateReset

    read  ow    = pushState ow stateRead

    write ow v  = pushTmp   ow v
               >> pushState ow stateWrite



instance Handler OW.HandleOneWire OneWire where
  addHandler _ = pure ()



push' :: KnownNat n => Queue n -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
push' q b v = push q $ \i -> store (b ! toIx i) v

pushState :: OneWire -> Uint8 -> Ivory eff ()
pushState OneWire{..} = push' stateQ stateB

pushTmp :: OneWire -> Uint8 -> Ivory eff ()
pushTmp OneWire{..} = push' tmpQ tmpB
