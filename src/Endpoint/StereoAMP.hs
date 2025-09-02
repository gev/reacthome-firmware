{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}


module Endpoint.StereoAMP where

import           Control.Monad.State
import           Core.Actions
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language


type StereoAMPStruct = "stereo_amp_struct"
type RulesAMPStruct = "rules_amp_struct"


[ivory|
    struct rules_amp_struct
    { isUsed     :: Array 9 (Stored IBool)
    ; volume     :: Array 9 (Stored IFloat)
    }
|]

[ivory|
    struct stereo_amp_struct
    { rules   :: Array 2 (Struct rules_amp_struct)
    ; mode    :: Uint8
    }
|]




-- message StereoAMP{..} i = do
--     pack   payload 0 actionLanamp
--     pack   payload 1 $ toIx i
--     pack   payload 2 0
--     pack   payload 3 0
--     pack   payload 4 =<< deref (channel ~> volume)
--     pack   payload 5 =<< deref (channel ~> volume)

-- size 41
-- lanamp:  ACTION_LANAMP index mode (2 byte volume??)  (active x 18) (volume x 18)
