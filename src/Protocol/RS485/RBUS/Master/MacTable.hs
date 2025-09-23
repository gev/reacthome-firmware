module Protocol.RS485.RBUS.Master.MacTable where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Version (Version, major, minor)
import Data.Record
import Data.Value
import GHC.TypeNats
import Interface.Mac (Mac)
import Ivory.Language
import Ivory.Language.Pointer
import Ivory.Language.Proc
import Ivory.Stdlib
import Util.String

type MacTableRecordStruct = "mac_table_record"
type MacTableRecord = Record MacTableRecordStruct
type MacTableRecords n = Records n MacTableRecordStruct

[ivory|
    struct mac_table_record
    { address :: Stored Uint8
    ; mac :: Array 6 (Stored Uint8)
    ; model :: Uint8
    ; version :: Struct version_struct
    }
|]

data MacTable = MacTable
    { name :: String
    , next :: Value Uint8
    , table :: Records 255 MacTableRecordStruct
    }

macTable :: (MonadState Context m) => String -> Int -> m MacTable
macTable id ports = do
    let name = id <> "_table"
    next <- value (name <> "_next") 0
    table <- records_ (name <> "_mac")
    addStruct (Proxy :: Proxy MacTableRecordStruct)
    pure MacTable{name, next, table}

insertMac ::
    MacTable ->
    Mac ->
    Value Uint8 ->
    Version ->
    (forall s. Mac -> Uint8 -> Value Uint8 -> Version -> forall s. Ivory (ProcEffects s ()) ()) ->
    Ivory (ProcEffects s ()) ()
insertMac MacTable{..} mac' model' version' run = do
    address <- local $ ival 255
    next' <- deref next
    for (toIx next') $ \ix -> do
        let cmp = 0
        cmp <- memCmp mac' $ table ! ix ~> mac
        when (cmp ==? 0) $ do
            store address $ safeCast ix
            breakOut
    address' <- deref address
    ifte_
        (address' ==? 255)
        ( do
            let address' = next'
            let rec' = table ! toIx address'
            memCpy (rec' ~> mac) mac'
            store (rec' ~> model) =<< deref model'
            store (rec' ~> version ~> major) =<< deref (version' ~> major)
            store (rec' ~> version ~> minor) =<< deref (version' ~> minor)
            store next $ next' + 1
            run mac' address' model' version'
        )
        ( do
            let rec' = table ! toIx address'
            store (rec' ~> model) =<< deref model'
            store (rec' ~> version ~> major) =<< deref (version' ~> major)
            store (rec' ~> version ~> minor) =<< deref (version' ~> minor)
            run mac' address' model' version'
        )

lookupMac ::
    MacTable ->
    Uint8 ->
    (MacTableRecord -> Ivory eff ()) ->
    Ivory eff ()
lookupMac MacTable{..} address run = do
    next' <- deref next
    when (address <? next') $
        run $
            table ! toIx address
