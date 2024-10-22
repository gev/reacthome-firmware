module Endpoint.ALED.Animation.Mask.Blink where

import           Data.Record
import           Endpoint.ALED
import           Endpoint.ALED.Animation.CosT
import           Endpoint.ALED.Animation.Data
import           Ivory.Language
import           Ivory.Stdlib


renderBlink :: IFloat
            -> Record AnimationStruct
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderBlink time = const $ do
    let i = castDefault $ 256 * time :: Sint32
    deref  $ addrOf cosT ! toIx i
