module Endpoint.ALED.Animation.Color.Fade where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib

renderFade ::
    IFloat ->
    Sint32 ->
    IFloat ->
    Record AnimationStruct ->
    IFloat ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderFade time subpixel value animation brightness = do
    v <- local $ ival value
    b <- deref $ animation ~> params ! toIx subpixel
    dt' <- deref $ animation ~> dt
    let b' = safeCast b * brightness
    let b'' = castDefault b' :: Uint8
    when (castDefault value /=? b'') do
        loop' <- deref $ animation ~> animationLoop
        ifte_
            loop'
            (store v b')
            ( do
                let rest = 1 - time
                ifte_
                    (rest >=? dt')
                    ( do
                        let delta = (b' - value) * dt' / rest
                        store v $ value + delta
                    )
                    (store v b')
            )
    deref v
