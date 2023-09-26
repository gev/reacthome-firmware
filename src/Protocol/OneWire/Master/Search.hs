{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.OneWire.Master.Search
    ( Search
    , mkSearch
    , search
    ) where

import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.FSM
import           Core.Task
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           GHC.TypeNats
import           Interface.OneWire
import           Ivory.Language
import           Ivory.Stdlib
import           Prelude               hiding (error, read)




stateSearch             = 0x00 :: Uint8
stateSearchNext         = 0x01 :: Uint8
stateReadIdBit          = 0x02 :: Uint8
stateReadCmpIdBit       = 0x03 :: Uint8
stateGetSearchDirection = 0x04 :: Uint8
stateWriteBit           = 0x05 :: Uint8
stateCheckDevices       = 0x06 :: Uint8
stateWrite              = 0x07 :: Uint8
stateReset              = 0x08 :: Uint8
stateWaitPresence       = 0x09 :: Uint8
stateWaitReady          = 0x0a :: Uint8
stateReady              = 0x0b :: Uint8
stateResult             = 0x0c :: Uint8
stateError              = 0x0d :: Uint8


timeReset               =  48 :: Uint8
timeWaitPresence        =  55 :: Uint8
timeWaitReady           =  97 :: Uint8
timeWrite0              =   7 :: Uint8
timeWrite1              =   1 :: Uint8
timeWriteSlot           =   8 :: Uint8
timeReadPrepare         =   1 :: Uint8
timeWaitBit             =   2 :: Uint8
timeReadSlot            =   8 :: Uint8


errorNoPresence         = 0x00 :: Uint8
errorNotReady           = 0x01 :: Uint8
errorCRC                = 0x02 :: Uint8

data Search = Search
    { onewire               :: OneWire
    , state                 :: Value       Uint8
    , time                  :: Value       Uint8
    , stateB                :: Buffer  32  Uint8
    , stateQ                :: Queue   32
    , width                 :: Value       Uint8
    , count                 :: Value       Uint8
    , countROM              :: Value       Uint8
    , savedROM              :: Buffer   8  Uint8
    , lastDiscrepancy       :: Value       Uint8
    , lastDeviceFlag        :: Value       IBool
    , lastFamilyDiscrepancy :: Value       Uint8
    , idBit                 :: Value       IBool
    , cmpIdBit              :: Value       IBool
    , idBitNumber           :: Value       Uint8
    , lastZero              :: Value       Uint8
    , romByteNumber         :: Value       Uint8
    , romByteMask           :: Value       Uint8
    , searchResult          :: Value       IBool
    , searchDirection       :: Value       IBool
    , error                 :: Value       Uint8
    , tmpV                  :: Value       Uint8
    , onData                :: forall   s. Uint8 -> Buffer 8 Uint8 -> Ivory (ProcEffects s ()) ()
    , onError               :: forall   s. Uint8 -> Ivory (ProcEffects s ()) ()

    }


mkSearch :: MonadState Context m
         => OneWire
         -> (forall s. Uint8 -> Buffer 8 Uint8 -> Ivory (ProcEffects s ()) ())
         -> (forall s. Uint8 -> Ivory (ProcEffects s ()) ())
         -> m Search
mkSearch onewire onData onError = do
    state                   <- value  "one_wire_state" stateReady
    time                    <- value  "one_wire_time"  0
    stateB                  <- buffer "one_wire_state"
    stateQ                  <- queue  "one_wire_state"
    width                   <- value_ "one_wire_bit_width"
    count                   <- value_ "one_wire_count"
    countROM                <- value_ "one_wire_count_rom"
    savedROM                <- buffer "one_wire_saved_rom"
    lastDiscrepancy         <- value_ "one_wire_last_discrepancy"
    lastDeviceFlag          <- value_ "one_wire_last_device_flag"
    lastFamilyDiscrepancy   <- value_ "one_wire_last_family_discrepancy"
    idBit                   <- value_ "one_wire_id_bit"
    cmpIdBit                <- value_ "one_wire_cmp_id_bit"
    idBitNumber             <- value_ "one_wire_id_bit_number"
    lastZero                <- value_ "one_wire_last_zero"
    romByteNumber           <- value_ "one_wire_rom_byte_number"
    romByteMask             <- value_ "one_wire_rom_byte_mask"
    searchResult            <- value_ "one_wire_search_result"
    searchDirection         <- value_ "one_wire_search_direction"
    error                   <- value_ "one_wire_error"
    tmpV                    <- value_ "one_wire_tmp_value"


    let search = Search { onewire
                        , state
                        , time
                        , stateB
                        , stateQ
                        , width
                        , count
                        , countROM
                        , savedROM
                        , lastDiscrepancy
                        , lastDeviceFlag
                        , lastFamilyDiscrepancy
                        , idBit
                        , cmpIdBit
                        , idBitNumber
                        , lastZero
                        , romByteNumber
                        , romByteMask
                        , searchResult
                        , searchDirection
                        , error
                        , onData
                        , onError
                        , tmpV
                        }

    addProc (getCRC :: GetCRC 8)

    handleTimer onewire $ handleOneWire search
    addTask $ yeld "one_wire" $ taskOneWire search

    pure search


taskOneWire :: Search -> Ivory (ProcEffects s ()) ()
taskOneWire = runState' state
    [ stateReady      |-> handleReady
    , stateSearch     |-> startSearch
    , stateSearchNext |-> searchNext
    , stateResult     |-> handleResult
    , stateError      |-> handleError
    ]


handleOneWire :: Search -> Ivory eff ()
handleOneWire = runState' state
    [ stateReset                |-> doReset
    , stateWaitPresence         |-> waitPresence
    , stateWaitReady            |-> waitReady
    , stateWrite                |-> doWrite
    , stateReadIdBit            |-> readBit idBit    stateReadCmpIdBit
    , stateReadCmpIdBit         |-> readBit cmpIdBit stateGetSearchDirection
    , stateGetSearchDirection   |-> getSearchDirection
    , stateWriteBit             |-> writeBit stateCheckDevices
    , stateCheckDevices         |-> checkDevices
    ]


{-
-   TODO: Should we start/stop timer?
-}


handleReady :: Search -> Ivory eff ()
handleReady m@Search {..} = popState m $ \nextState ->
    cond_ [ nextState ==? stateSearch ==> store state stateSearch
          ]


startSearch :: Search -> Ivory eff ()
startSearch Search{..} = do
    arrayMap $ \ix -> store (savedROM ! ix) 0
    store countROM              0
    store lastDiscrepancy       0
    store lastFamilyDiscrepancy 0
    store lastDeviceFlag        false
    store state stateSearchNext


searchNext :: Search -> Ivory eff ()
searchNext Search{..} = do
    lastDeviceFlag'  <- deref lastDeviceFlag
    ifte_ lastDeviceFlag'
        (store state stateReady)
        (do
            store idBitNumber   1
            store lastZero      0
            store romByteNumber 0
            store romByteMask   1
            store searchResult  false
            store state stateReset
        )


handleResult :: Search -> Ivory (ProcEffects s ()) ()
handleResult Search{..} = do
    countROM' <- deref countROM
    crc <- call getCRC savedROM
    ifte_ (crc ==? 0)
          (do
            onData countROM' savedROM
            ifte_ (countROM' ==? 255)
                (store state stateReady)
                (do
                    store countROM $ countROM' + 1
                    store state stateSearchNext
                )
          )
          (do
               store error errorCRC
               store state stateError
          )


handleError :: Search -> Ivory (ProcEffects s ()) ()
handleError m@Search {..} = do
    onError =<< deref error
    popState m $ \nextState ->
        when (nextState ==? stateReset .|| nextState ==? stateSearch) $ do
            store time 0
            store state nextState



doReset :: Search -> Ivory eff ()
doReset Search{..} = do
    time' <- deref time
    cond_
        [ time' ==? 0 ==> do
            pullDown onewire
            store time 1
        , time' ==? timeReset ==> do
            pullUp onewire
            store state stateWaitPresence
        , true ==> store time (time' + 1)
        ]


waitPresence :: Search -> Ivory eff ()
waitPresence Search{..} = do
    time' <- deref time
    cond_
        [ time' ==? timeWaitPresence ==> do
            hasPresence <- iNot <$> getState onewire
            ifte_ hasPresence
                (store state stateWaitReady)
                (do
                    store error errorNoPresence
                    store state stateError
                )
        , true ==> store time (time' + 1)
        ]


waitReady :: Search -> Ivory eff ()
waitReady Search{..} = do
    time' <- deref time
    cond_
        [ time' ==? timeWaitReady ==> do
            hasPresence <- getState onewire
            ifte_ hasPresence
                (do
                    store tmpV  0xf0
                    store count 0
                    store time  0
                    store state stateWrite)
                (do
                    store error errorNotReady
                    store state stateError
                )
        , true ==> store time (time' + 1)
        ]


doWrite :: Search -> Ivory eff ()
doWrite Search{..} = do
    time'  <- deref time
    width' <- deref width
    cond_
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
                (do
                        store time 0
                        store state stateReadIdBit
                )
        , time' ==? width' ==> pullUp onewire >> store time (time' + 1)
        , time' ==? timeWriteSlot ==> store time 0
        , true  ==> store time (time' + 1)
        ]


readBit :: (Search -> Value IBool) -> Uint8 -> Search -> Ivory eff ()
readBit bit nextState s@Search{..} = do
    time'  <- deref time
    cond_
        [ time' ==? 0 ==> do
            pullDown onewire
            store time 1
        , time' ==? timeReadPrepare ==> do
            pullUp onewire
            store time (time' + 1)
        , time' ==? timeWaitBit ==> do
            store (bit s) =<< getState onewire
            store time $ time' + 1
        , time' ==? timeReadSlot ==> do
            store time  0
            store state nextState
        , true  ==> store time (time' + 1)
        ]


writeBit :: Uint8 -> Search -> Ivory eff ()
writeBit nextState Search{..} = do
    time'  <- deref time
    width' <- deref width
    cond_
        [ time' ==? 0 ==> pullDown onewire >> store time 1
        , time' ==? width' ==> pullUp onewire >> store time (time' + 1)
        , time' ==? timeWriteSlot ==> do
            store time  0
            store state nextState
        , true  ==> store time (time' + 1)
        ]


getSearchDirection :: Search -> Ivory eff ()
getSearchDirection Search{..} = do
    idBit' <- deref idBit
    cmpIdBit' <- deref cmpIdBit
    ifte_ (idBit' .&& cmpIdBit')
          (store state stateReady)
          (do
                idBitNumber'   <- deref idBitNumber
                romByteMask'   <- deref romByteMask
                romByteNumber' <- deref romByteNumber
                let rom         = savedROM ! toIx romByteNumber'
                rom'           <- deref rom
                ifte_ (idBit' /=? cmpIdBit')
                      (store searchDirection idBit')
                      (do
                            lastDiscrepancy' <- deref lastDiscrepancy
                            ifte_ (idBitNumber' <? lastDiscrepancy')
                                  (store searchDirection $ rom' .& romByteMask' >? 0)
                                  (store searchDirection $ idBitNumber' ==? lastDiscrepancy')
                            searchDirection' <- deref searchDirection
                            when (iNot searchDirection') $ do
                                store lastZero idBitNumber'
                                when (idBitNumber' <? 9)  $ do
                                    store lastFamilyDiscrepancy idBitNumber'

                      )
                searchDirection' <- deref searchDirection
                rom' <- deref rom
                ifte_ searchDirection'
                      (do
                            store rom $ rom' .| romByteMask'
                            store width timeWrite1
                      )
                      (do
                            store rom $ rom' .& iComplement romByteMask'
                            store width timeWrite0
                    )
                store state stateWriteBit
          )


checkDevices :: Search -> Ivory eff ()
checkDevices Search{..} = do
    idBitNumber'   <- deref idBitNumber
    romByteMask'   <- deref romByteMask
    romByteNumber' <- deref romByteNumber
    store idBitNumber $ idBitNumber' + 1
    store romByteMask $ romByteMask' `iShiftL` 1
    romByteMask'' <- deref romByteMask
    when (romByteMask'' ==? 0) $ do
        store romByteNumber $ romByteNumber' + 1
        store romByteMask 1
    romByteNumber'' <- deref romByteNumber
    ifte_ (romByteNumber'' ==? 8)
          (do
                idBitNumber'' <- deref idBitNumber
                when (idBitNumber'' >? 64) $ do
                    lastZero' <- deref lastZero
                    store lastDiscrepancy lastZero'
                    when (lastZero' ==? 0) $ store lastDeviceFlag true
                    store searchResult true
                searchResult' <- deref searchResult
                rom' <- deref (savedROM ! 0)
                ifte_ (searchResult' .&& rom' /=? 0)
                      (store state stateResult)
                      (do
                            store error errorCRC
                            store state stateError
                      )
          )
          (store state stateReadIdBit)



search :: Search -> Ivory eff ()
search m = pushState m stateSearch



type GetCRC n = Def ('[Buffer n Uint8] :-> Uint8)

getCRC :: KnownNat n => GetCRC n
getCRC = proc "one_wire_get_crc" $ \buff -> body $ do
    crc <- local $ ival 0
    arrayMap $ \ix -> do
        inbyte <- local . ival =<< deref (buff ! ix)
        for (8 :: Ix 8) . const $ do
            crc' <- deref crc
            inbyte' <- deref inbyte
            let mix = (crc' .^ inbyte') .& 0x01
            store crc $ crc' `iShiftR` 1
            when (mix /=? 0) $ do
                crc'' <- deref crc
                store crc $ crc'' .^ 0x8c
            store inbyte $ inbyte' `iShiftR` 1
    ret =<< deref crc



pop' :: KnownNat n => Queue n -> Buffer n Uint8 -> (Uint8 -> Ivory eff ())-> Ivory eff ()
pop' q b run = pop q $ \i -> run =<< deref (b ! toIx i)

popState :: Search -> (Uint8 -> Ivory eff ()) -> Ivory eff ()
popState Search{..} = pop' stateQ stateB


push' :: KnownNat n => Queue n -> Buffer n Uint8 -> Uint8 -> Ivory eff ()
push' q b v = push q $ \i -> store (b ! toIx i) v

pushState :: Search -> Uint8 -> Ivory eff ()
pushState Search{..} = push' stateQ stateB
