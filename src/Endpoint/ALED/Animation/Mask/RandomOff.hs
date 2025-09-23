module Endpoint.ALED.Animation.Mask.RandomOff where

import Data.Record
import Endpoint.ALED
import Endpoint.ALED.Animation.Data
import Ivory.Language
import Ivory.Stdlib
import Util.Random

renderRandomOff ::
   IFloat ->
   Sint32 ->
   Random Uint8 ->
   Record AnimationStruct ->
   Ivory (AllowBreak (ProcEffects s ())) IFloat
renderRandomOff time pixel random animation = do
   let x = pixel .% 96
   let px = toIx $ x `iDiv` 12
   let b = castDefault $ pixel .% 8
   let p = animation ~> params ! px
   startLoop' <- deref $ animation ~> startLoop
   when startLoop' $ store p 0
   p' <- deref p
   let isOff = (p' `iShiftR` b) .& 1
   ifte
      (isOff ==? 0)
      ( do
         dt' <- deref $ animation ~> dt
         t <- next random
         ifte
            (safeCast t / 255 <? time + dt')
            ( do
               store p $ p' .| (1 `iShiftL` b)
               pure 255
            )
            (pure 1)
      )
      (pure 0)
