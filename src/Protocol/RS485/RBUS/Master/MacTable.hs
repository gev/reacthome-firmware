{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Protocol.RS485.RBUS.Master.MacTable where

import           Control.Monad.Writer   (MonadWriter)
import           Core.Context
import           Core.Version
import           Data.Matrix
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Pointer
import           Ivory.Language.Proc
import           Ivory.Stdlib
import           Util.String



data MacTable = MacTable
    { name     :: String
    , size     :: Value          Sint32
    , tAddress :: Values   255   (Ix 255)
    , tIndex   :: Values   255   (Ix 255)
    , tMac     :: Matrix   255 6 Uint8
    , tModel   :: Values   255   Uint8
    , tVersion :: Versions 255
    }



macTable :: MonadWriter Context m => String -> m MacTable
macTable id = do
    let name = id <> "_table"
    size     <- value     (name <> "_size"   ) 0
    tAddress <- values_   (name <> "_address")
    tIndex   <- values_   (name <> "_index"  )
    tMac     <- matrix_   (name <> "_mac"    )
    tModel   <- values_   (name <> "_model"  )
    tVersion <- versions_ (name <> "_version")
    addModule . incl $ getIndex
    addModule . incl $ insertMac
    pure MacTable {name, size, tAddress, tIndex, tMac, tModel, tVersion}



getIndex :: Def ('[Matrix 255 6 Uint8, Value Sint32, Mac] :-> Sint32)
getIndex = proc "mac_table_get_index" $ \table size mac -> body $ do
        size' <- deref size
        mid   <- local $ ival 0
        left  <- local $ ival 0
        right <- local $ ival size'
        forever $ do
            left'  <- deref left
            right' <- deref right
            ifte_
                (left' <? right')
                (do
                    store mid $ (left' + right') ./ 2
                    mid' <- deref mid
                    cmp' <- memCmp (table ! toIx mid') mac
                    cond_   [ cmp' <? 0 ==> store left (mid' + 1)
                            , cmp' >? 0 ==> store right mid'
                            , true      ==> ret mid'
                            ]
                )
                breakOut
        left' <- deref left
        ret (-left')



insertMac :: Def ('[Matrix 255 6 Uint8, Ix 255, Mac] :-> ())
insertMac = proc "mac_table_insert_mac" $ \table index mac -> body $ do
    memCpy (table ! index) mac
    pure ()



shiftMac :: Def ('[Matrix 255 6 Uint8, Ix 255, Mac] :-> ())
shiftMac = proc "mac_table_shift_mac" $ \table index mac -> body $ do
    memCpy (table ! index) mac
    pure ()




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
runMac :: MacTable -> Mac -> (Ix 255 -> Ivory eff ()) -> Ivory eff ()
runMac MacTable{..} mac run = do
    s <- deref size
    when (s >? 0) $ do
        i <- call getIndex tMac size mac
        when (i >=? 0) $
            run =<< deref (tAddress ! toIx i)



runAddress :: MacTable -> Ix 255 -> (Mac -> Ivory eff ()) -> Ivory eff ()
runAddress MacTable{..} address run = do
    s <- deref size
    when (s >? 0) $ do
        i <- deref (tIndex ! address)
        when (i >=? 0) $
            run $ tMac ! toIx i
