{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Protocol.OneWire.Master
    ( OneWireMaster
    , mkOneWireMaster
    , reset
    , write
    , read
    , search
    , skipROM
    , matchROM
    , errorNoPresence
    , errorNotReady
    , errorCRC
    ) where

import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.FSM
import           Core.Task             (yeld)
import           Data.Buffer
import           Data.Queue
import           Data.Record
import           Data.Value
import           Feature.RS485.RBUS.Tx (run)
import           GHC.TypeNats
import           Interface.OneWire
import           Ivory.Language
import           Ivory.Stdlib
import           Prelude               hiding (error, read)



actionWrite             = 0x0 :: Uint8
actionRead              = 0x1 :: Uint8
actionReset             = 0x2 :: Uint8
actionSearch            = 0x3 :: Uint8


stateWrite              = 0x0 :: Uint8
stateRead               = 0x1 :: Uint8
stateReset              = 0x2 :: Uint8
stateWaitPresence       = 0x3 :: Uint8
stateWaitReady          = 0x4 :: Uint8
stateReady              = 0x5 :: Uint8
stateSearchNext         = 0x6 :: Uint8
stateReadIdBit          = 0x7 :: Uint8
stateReadCmpIdBit       = 0x8 :: Uint8
stateGetSearchDirection = 0x9 :: Uint8
stateWriteBit           = 0xa :: Uint8
stateCheckDevices       = 0xb :: Uint8
stateReadResult         = 0xc :: Uint8
stateSearchResult       = 0xd :: Uint8
stateError              = 0xe :: Uint8
stateRecovery           = 0xf :: Uint8


timeReset               =  48 :: Uint8
timeWaitPresence        =  55 :: Uint8
timeWaitReady           =  97 :: Uint8
timeWrite0              =   7 :: Uint8
timeWrite1              =   1 :: Uint8
timeWriteSlot           =   8 :: Uint8
timeReadPrepare         =   1 :: Uint8
timeWaitBit             =   2 :: Uint8
timeReadSlot            =   8 :: Uint8


errorNoPresence         = 0x1 :: Uint8
errorNotReady           = 0x2 :: Uint8
errorCRC                = 0x3 :: Uint8



type OneWireAction = "one_wire_action_struct"

[ivory|
    struct one_wire_action_struct {
        action_  :: Uint8;
        payload_ :: Uint8;
        index_   :: Uint8;
    }
|]



data OneWireMaster = OneWireMaster
    { onewire               :: OneWire
    , state                 :: Value        Uint8
    , action                :: Value        Uint8
    , payload               :: Value        Uint8
    , index                 :: Value        Uint8
    , time                  :: Value        Uint8
    , actionB               :: Records  24  OneWireAction
    , actionQ               :: Queue    24
    , tmp                   :: Value        Uint8
    , width                 :: Value        Uint8
    , count                 :: Value        Uint8
    , countROM              :: Value        Uint8
    , savedROM              :: Buffer    8  Uint8
    , lastDiscrepancy       :: Value        Uint8
    , lastDeviceFlag        :: Value        IBool
    , lastFamilyDiscrepancy :: Value        Uint8
    , idBit                 :: Value        IBool
    , cmpIdBit              :: Value        IBool
    , idBitNumber           :: Value        Uint8
    , lastZero              :: Value        Uint8
    , romByteNumber         :: Value        Uint8
    , romByteMask           :: Value        Uint8
    , searchResult          :: Value        IBool
    , searchDirection       :: Value        IBool
    , error                 :: Value        Uint8
    , onData                :: forall    s. Uint8 -> Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
    , onDiscovery           :: forall    s. Uint8 -> Buffer 8 Uint8 -> Ivory (ProcEffects s ()) ()
    , onError               :: forall    s. Uint8 -> Ivory (ProcEffects s ()) ()
    }



mkOneWireMaster :: MonadState Context m
                => OneWire
                -> (forall s. Uint8 -> Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ())
                -> (forall s. Uint8 -> Buffer 8 Uint8 -> Ivory (ProcEffects s ()) ())
                -> (forall s. Uint8 -> Ivory (ProcEffects s ()) ())
                -> m OneWireMaster
mkOneWireMaster onewire onData onDiscovery onError = do
    state                   <- value    "one_wire_state" stateReady
    action                  <- value_   "one_wire_action"
    payload                 <- value_   "one_wire_payload"
    index                   <- value_   "one_wire_index"
    time                    <- value    "one_wire_time" 0
    actionB                 <- records_ "one_wire_actions"
    actionQ                 <- queue    "one_wire_action"
    tmp                     <- value_   "one_wire_tmp"
    width                   <- value_   "one_wire_bit_width"
    count                   <- value_   "one_wire_count"
    countROM                <- value_   "one_wire_count_rom"
    savedROM                <- buffer   "one_wire_saved_rom"
    lastDiscrepancy         <- value_   "one_wire_last_discrepancy"
    lastDeviceFlag          <- value_   "one_wire_last_device_flag"
    lastFamilyDiscrepancy   <- value_   "one_wire_last_family_discrepancy"
    idBit                   <- value_   "one_wire_id_bit"
    cmpIdBit                <- value_   "one_wire_cmp_id_bit"
    idBitNumber             <- value_   "one_wire_id_bit_number"
    lastZero                <- value_   "one_wire_last_zero"
    romByteNumber           <- value_   "one_wire_rom_byte_number"
    romByteMask             <- value_   "one_wire_rom_byte_mask"
    searchResult            <- value_   "one_wire_search_result"
    searchDirection         <- value_   "one_wire_search_direction"
    error                   <- value_   "one_wire_error"

    let master  = OneWireMaster { onewire
                                , state
                                , action
                                , payload
                                , index
                                , time
                                , actionB
                                , actionQ
                                , tmp
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
                                , onDiscovery
                                , onError
                                }

    addProc getCRC
    addStruct (Proxy :: Proxy OneWireAction)

    handleTimer onewire $ handleOneWire master
    addTask $ yeld "one_wire" $ taskOneWire master

    pure master



reset :: OneWireMaster -> Ivory eff ()
reset m = pushAction m actionReset 0 0

read :: OneWireMaster -> Uint8 -> Uint8 -> Ivory eff ()
read m = pushAction m actionRead

write :: OneWireMaster -> Uint8 -> Ivory eff ()
write m v = pushAction m actionWrite v 0

search :: OneWireMaster -> Uint16 -> Ivory eff ()
search m n = pushAction m actionSearch (castDefault $ n - 1) 0

skipROM :: OneWireMaster -> Ivory eff ()
skipROM m = write m 0xcc

matchROM :: OneWireMaster -> Ivory eff ()
matchROM m = write m 0x55



taskOneWire :: OneWireMaster -> Ivory (ProcEffects s ()) ()
taskOneWire = runState' state
        [ stateReady        |-> handleReady
        , stateSearchNext   |-> handleSearchNext
        , stateReadResult   |-> handleReadResult
        , stateSearchResult |-> handleSearchResult
        , stateError        |-> handleError
        , stateRecovery     |-> recovery
        ]



handleReady :: OneWireMaster -> Ivory eff ()
handleReady m =
    popAction m $ \action' payload' index' -> do
        store (action  m) action'
        store (payload m) payload'
        store (index   m) index'
        runAction m


handleSearchNext :: OneWireMaster -> Ivory eff ()
handleSearchNext OneWireMaster{..} = do
    lastDeviceFlag'  <- deref lastDeviceFlag
    ifte_ lastDeviceFlag'
        (store state stateReady)
        (do
            store idBitNumber   1
            store lastZero      0
            store romByteNumber 0
            store romByteMask   1
            store searchResult  false
            store count         0
            store state         stateReset
        )


handleReadResult :: OneWireMaster -> Ivory (ProcEffects s ()) ()
handleReadResult OneWireMaster{..} = do
    payload' <- deref payload
    index'   <- deref index
    onData payload' index' =<< deref tmp
    store state stateReady


handleSearchResult :: OneWireMaster -> Ivory (ProcEffects s ()) ()
handleSearchResult OneWireMaster{..} = do
    countROM' <- deref countROM
    crc <- call getCRC savedROM
    ifte_ (crc ==? 0)
          (do
            onDiscovery countROM' savedROM
            n <- deref payload
            ifte_ (countROM' ==? n)
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



handleError :: OneWireMaster -> Ivory (ProcEffects s ()) ()
handleError OneWireMaster {..} = do
    onError =<< deref error
    store state stateRecovery


recovery :: OneWireMaster -> Ivory (ProcEffects s ()) ()
recovery m@OneWireMaster {..} =
    popAction m $ \action' payload' index' ->
        when (action' ==? actionReset .|| action' ==? actionSearch) $ do
            store action action'
            runAction m


runAction :: OneWireMaster -> Ivory eff ()
runAction = runState' action
    [ actionWrite  |-> initWriteAction
    , actionRead   |-> initReadAction
    , actionSearch |-> initSearchAction
    , actionReset  |-> initResetAction
    ]


initWriteAction :: OneWireMaster -> Ivory eff ()
initWriteAction m@OneWireMaster{..} = do
    store tmp   =<< deref payload
    store count 0
    store state stateWrite


initReadAction :: OneWireMaster -> Ivory eff ()
initReadAction OneWireMaster{..} = do
    store tmp   0
    store count 0
    store state stateRead


initSearchAction :: OneWireMaster -> Ivory eff ()
initSearchAction OneWireMaster{..} = do
    arrayMap $ \ix -> store (savedROM ! ix) 0
    store countROM              0
    store lastDiscrepancy       0
    store lastFamilyDiscrepancy 0
    store lastDeviceFlag        false
    store tmp                   0xf0
    store state                 stateSearchNext


initResetAction :: OneWireMaster -> Ivory eff ()
initResetAction OneWireMaster{..} =
    store state stateReset



handleOneWire :: OneWireMaster -> Ivory eff ()
handleOneWire = runState' action
    [ actionReset  |-> runResetAction
    , actionWrite  |-> runWriteAction
    , actionRead   |-> runReadAction
    , actionSearch |-> runSearchAction
    ]


runResetAction :: OneWireMaster -> Ivory eff ()
runResetAction = runState' state
    [ stateReset        |-> doReset
    , stateWaitPresence |-> waitPresence
    , stateWaitReady    |-> waitReady stateReady
    ]


runWriteAction :: OneWireMaster -> Ivory eff ()
runWriteAction = runState' state
    [ stateWrite |-> doWrite stateReady
    ]


runReadAction :: OneWireMaster -> Ivory eff ()
runReadAction = runState' state
    [ stateRead |-> doRead stateReadResult
    ]


runSearchAction :: OneWireMaster -> Ivory eff ()
runSearchAction = runState' state
    [ stateReset               |-> doReset
    , stateWaitPresence        |-> waitPresence
    , stateWaitReady           |-> waitReady          stateWrite
    , stateWrite               |-> doWrite            stateReadIdBit
    , stateReadIdBit           |-> readBit            idBit     stateReadCmpIdBit
    , stateReadCmpIdBit        |-> readBit            cmpIdBit  stateGetSearchDirection
    , stateGetSearchDirection  |-> getSearchDirection
    , stateWriteBit            |-> writeBit           stateCheckDevices
    , stateCheckDevices        |-> checkDevices
    ]


doReset :: OneWireMaster -> Ivory eff ()
doReset OneWireMaster{..} = do
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


waitPresence :: OneWireMaster -> Ivory eff ()
waitPresence OneWireMaster{..} = do
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


waitReady :: Uint8 -> OneWireMaster -> Ivory eff ()
waitReady nextState OneWireMaster{..} = do
    time' <- deref time
    cond_
        [ time' ==? timeWaitReady ==> do
            hasPresence <- getState onewire
            ifte_ hasPresence
                (do
                    store time  0
                    store state nextState)
                (do
                    store error errorNotReady
                    store state stateError
                )
        , true ==> store time (time' + 1)
        ]

doWrite :: Uint8 -> OneWireMaster -> Ivory eff ()
doWrite nextState OneWireMaster{..} = do
    time'  <- deref time
    width' <- deref width
    cond_
        [ time' ==? 0 ==> do
            count' <- deref count
            ifte_ (count' <? 8)
                (do
                        pullDown onewire
                        tmp' <- deref tmp
                        let bit = (tmp' `iShiftR` count') .& 1
                        ifte_ (bit ==? 1)
                            (store width timeWrite1)
                            (store width timeWrite0)
                        store count $ count' + 1
                        store time 1
                )
                (store state nextState)
        , time' ==? width' ==> pullUp onewire >> store time (time' + 1)
        , time' ==? timeWriteSlot ==> store time 0
        , true  ==> store time (time' + 1)
        ]


doRead :: Uint8 -> OneWireMaster -> Ivory eff ()
doRead nextState OneWireMaster{..} = do
    time' <- deref time
    cond_
        [ time' ==? 0 ==> do
            pullDown onewire
            store time 1
        , time' ==? timeReadPrepare ==> do
            pullUp onewire
            store time (time' + 1)
        , time' ==? timeWaitBit ==> do
            bit    <- getState onewire
            tmp'   <- deref tmp
            count' <- deref count
            store tmp $ tmp' .| (safeCast bit `iShiftL` count')
            store count $ count' + 1
            store time $ time' + 1
        , time' ==? timeReadSlot ==> do
            count' <- deref count
            when (count' ==? 8) $ do
                  store state nextState
            store time 0
        , true  ==> store time (time' + 1)
        ]


readBit :: (OneWireMaster -> Value IBool) -> Uint8 -> OneWireMaster -> Ivory eff ()
readBit bit nextState s@OneWireMaster{..} = do
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


writeBit :: Uint8 -> OneWireMaster -> Ivory eff ()
writeBit nextState OneWireMaster{..} = do
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


getSearchDirection :: OneWireMaster -> Ivory eff ()
getSearchDirection OneWireMaster{..} = do
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


checkDevices :: OneWireMaster -> Ivory eff ()
checkDevices OneWireMaster{..} = do
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
                      (store state stateSearchResult)
                      (do
                            store error errorCRC
                            store state stateError
                      )
          )
          (store state stateReadIdBit)





getCRC :: Def ('[Buffer 8 Uint8] :-> Uint8)
getCRC = proc "one_wire_get_crc" $ \buff -> body $ do
    crc <- local $ ival 0
    arrayMap $ \ix -> do
        inbyte <- local . ival =<< deref (buff ! ix)
        times (8 :: Ix 9) . const $ do
            crc' <- deref crc
            inbyte' <- deref inbyte
            let mix = (crc' .^ inbyte') .& 0x01
            store crc $ crc' `iShiftR` 1
            when (mix /=? 0) $ do
                crc'' <- deref crc
                store crc $ crc'' .^ 0x8c
            store inbyte $ inbyte' `iShiftR` 1
    ret =<< deref crc



popAction :: OneWireMaster -> (Uint8 -> Uint8 -> Uint8 -> Ivory eff ()) -> Ivory eff ()
popAction OneWireMaster{..} run =
    flip (pop' actionQ) (disableOneWire onewire) $ \ix -> do
        let a = actionB ! ix
        action'  <- deref (a ~> action_ )
        payload' <- deref (a ~> payload_)
        index'   <- deref (a ~> index_  )
        run action' payload' index'


pushAction :: OneWireMaster -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
pushAction OneWireMaster{..} action' payload' index' =
    push actionQ $ \ix -> do
        let a = actionB ! ix
        store (a ~> action_ ) action'
        store (a ~> payload_) payload'
        store (a ~> index_  ) index'
        enableOneWire onewire
