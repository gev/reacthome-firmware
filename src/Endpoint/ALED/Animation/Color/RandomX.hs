module Endpoint.ALED.Animation.Color.RandomX where

import Data.Record
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib
import Util.Random

renderRandomX ::
    Sint32 ->
    IFloat ->
    Random Uint8 ->
    Record AnimationStruct ->
    IFloat ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomX subpixel value random animation brightness = do
    i <- next random
    dt' <- deref $ animation ~> dt
    (* brightness)
        <$> ifte
            (safeCast i / 255 <=? dt' * safeCast fps)
            do
                min <- safeCast <$> deref (animation ~> params ! toIx (2 * subpixel))
                max <- safeCast <$> deref (animation ~> params ! toIx (2 * subpixel + 1))
                r <- safeCast <$> next random
                pure $ brightness * (r * (max - min) / 255 + min)
            do
                pure value
