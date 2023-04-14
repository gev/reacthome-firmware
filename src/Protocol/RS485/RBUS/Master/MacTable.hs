{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Protocol.RS485.RBUS.Master.MacTable where

import           Control.Monad.Writer   (MonadWriter)
import           Core.Context
import           Core.Version           (Version, major, minor)
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac          (Mac)
import           Ivory.Language
import           Ivory.Language.Pointer
import           Ivory.Language.Proc
import           Ivory.Stdlib
import           Util.String



type MacTableRecordStruct = "mac_table_record"
type MacTableRecord = Record MacTableRecordStruct
type MacTableRecords n = Records n MacTableRecordStruct

[ivory|
    struct mac_table_record
    { port    :: Stored Uint8
    ; address :: Stored Uint8
    ; mac     :: Array 6 (Stored Uint8)
    ; model   :: Uint8
    ; version :: Struct version_struct
    }
|]



data MacTable = MacTable
    { name     :: String
    , ports    :: Int
    , size     :: Value      Uint16
    , runNext  :: RunValues  Uint8
    , runIndex :: RunValues  Uint16
    , runMac   :: RunRecords MacTableRecordStruct
    }



macTable :: MonadWriter Context m => String -> Int -> m MacTable
macTable id ports = do
    let name      = id <> "_table"
    let tSize     = 255 * ports
    let runNext   = runValues   (name <> "_next"   ) (replicate ports 0)
    let runIndex  = runValues_  (name <> "_index"  ) tSize
    let runMac    = runRecords_ (name <> "_mac"    ) tSize
    size         <- value       (name <> "_size"   ) 0

    runNext  addArea
    runIndex addArea
    runMac   addArea

    pure MacTable { name, ports, size, runNext, runIndex, runMac }


getIndex :: (KnownNat m, KnownNat n)
         => MacTableRecords n
         -> Values m Uint16
         -> Value Uint16
         -> Mac
         -> Ivory (ProcEffects s ()) Sint32
getIndex table index size mac' = do
    size' <- deref size
    mid   <- local $ ival 0
    left  <- local $ ival 0
    right <- local $ ival size'
    res   <- local $ ival (-1 :: Sint32)
    forever $ do
        left'  <- deref left
        right' <- deref right
        ifte_
            (left' <? right')
            (do
                store mid $ (left' + right') ./ 2
                mid'   <- deref mid
                index' <- deref $ index ! toIx mid'
                cmp'   <- memCmp ((table ! toIx index') ~> mac)  mac'
                cond_ [ cmp' <? 0 ==> store left (mid' + 1)
                      , cmp' >? 0 ==> store right mid'
                      , true      ==> store res (safeCast mid') >> breakOut
                      ]
            )
            (do
                store res (- (safeCast $ left' + 1))
                breakOut
            )
    deref res



insertMac :: MacTable
          -> Uint8
          -> Mac
          -> Value Uint8
          -> Version
          -> (Uint8 -> Ivory (ProcEffects s ()) ())
          -> Ivory (ProcEffects s ()) ()
insertMac MacTable{..} port mac' model' version' run = do
    runMac $ \table -> do
        let table' = addrOf table
        runIndex $ \index -> do
            let index' = addrOf index
            i <- getIndex table' index' size mac'
            ifte_
                (i >=? 0)
                (do
                    let rec = table' ! toIx i
                    address' <- deref $ rec ~> address
                    run address'
                )
                (do
                    let dst =     - i
                    let src = dst - 1
                    runNext $ \next -> do
                        size' <- deref size
                        let next' = addrOf next ! toIx port
                        address' <- deref next'
                        runIndex $ \index -> do
                            let index' = addrOf index ! toIx (mkIndex port address')
                            store index' size'
                            runMac $ \rec -> do
                                let rec' = addrOf rec ! toIx size'
                                memCpy (rec' ~> mac  ) mac'
                                store  (rec' ~> model) =<< deref model'
                                store  (rec' ~> version ~> major) =<< deref (version' ~> major)
                                store  (rec' ~> version ~> minor) =<< deref (version' ~> minor)
                        store next' $ address' + 1
                        store size  $ size'    + 1
                        run address'
                )


-- int16_t rbus_mac_table_add_mac(rbus_mac_table_t *rbus_mac_table, mac_t *mac, rbus_device_type_t *type)
-- {
--     if (rbus_mac_table->number == RBUS_MAC_NUMBER)
--     {
--         return -1;
--     }

--     int16_t index = rbus_mac_table_get_index(rbus_mac_table, mac);

--     if (index < 0)
--     {
--         uint16_t dst = abs(index);
--         uint16_t src = dst - 1;
--         uint16_t address = rbus_mac_table->number++;
--         rbus_mac_table_record_t *record = &(rbus_mac_table->address_index[address]);
--         memcpy(&(record->type), type, sizeof(rbus_device_type_t));
--         memcpy(&(record->mac), mac, sizeof(mac_t));
--         record->address = address;
--         if (address > src)
--         {
--             memmove(&(rbus_mac_table->mac_index[dst]), &(rbus_mac_table->mac_index[src]), (address - src) * sizeof(rbus_mac_table_record_t*));
--         }
--         rbus_mac_table->mac_index[src] = record;
--         return address;
--     }
--     else
--     {
--         return rbus_mac_table->mac_index[index]->address;
--     }
-- }
lookupAddress :: MacTable
              -> Mac
              -> (Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ())
              -> Ivory (ProcEffects s ()) ()
lookupAddress MacTable{..} mac run =
    runMac $ \table -> do
        let table' = addrOf table
        runIndex $ \index -> do
            let index' = addrOf index
            i <- getIndex table' index' size mac
            when (i >=? 0) $ do
                let rec   = table' ! toIx i
                port'    <- deref $ rec ~> port
                address' <- deref $ rec ~> address
                run port' address'



lookupMac :: MacTable
          -> Uint8
          -> Uint8
          -> (MacTableRecord -> Ivory eff ())
          -> Ivory eff ()
lookupMac MacTable{..} port address run =
    runNext $ \next -> do
        next' <- deref $ addrOf next ! toIx port
        when (address <? next') $ do
            runIndex $ \index -> do
                i <- deref $ addrOf index ! toIx (mkIndex port address)
                runMac $ \mac ->
                    run $ addrOf mac ! toIx i


mkIndex :: Uint8 -> Uint8 -> Uint16
mkIndex port address = safeCast port `iShiftL` 8 .| safeCast address
