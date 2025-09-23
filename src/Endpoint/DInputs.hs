
module Endpoint.DInputs where

import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Data.Buffer
import Data.Record
import Data.Serialize
import GHC.TypeNats
import Ivory.Language

type DInputStruct = "dinput_struct"

[ivory|
    struct dinput_struct
    { state :: IBool
    ; timestamp :: Uint32
    ; synced :: IBool
    }
|]

data DInputs n = DInputs
    { dinputs :: Records n DInputStruct
    , payload :: Buffer 3 Uint8
    }

mkDinputs :: (MonadState Context m, KnownNat n) => String -> m (DInputs n)
mkDinputs name = do
    addStruct (Proxy :: Proxy DInputStruct)
    dinputs <-
        records'
            name
            [ state .= ival false
            , timestamp .= ival 0
            , synced .= ival false
            ]
    payload <- buffer "dinput_message"
    pure DInputs{dinputs, payload}

message :: (KnownNat n) => DInputs n -> Uint8 -> Ivory eff (Buffer 3 Uint8)
message DInputs{..} i = do
    let dinput = dinputs ! toIx i
    pack payload 0 actionDi
    pack payload 1 $ i + 1
    pack payload 2 =<< deref (dinput ~> state)
    pure payload
