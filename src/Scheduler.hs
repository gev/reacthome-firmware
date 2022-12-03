{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Scheduler where

import           Feature
import           Interface       as I
import           Interface.IRQ   as Q
import           Interface.Timer as I
import           Ivory.Language


data Scheduler a = I.Timer a => Scheduler a

instance I.Interface (Scheduler t) where

  dependencies (Scheduler t) = defMemArea clock
                             : I.dependencies t
                            <> Q.dependencies t handleIRQ

  initialize (Scheduler t) = I.initialize t
                          <> Q.initialize t

clock :: MemArea ('Stored Uint32)
clock = area "scheduler_clock" (Just (ival 0))

handleIRQ :: Ivory (ProcEffects s ()) ()
handleIRQ = do
  let c = addrOf clock
  v <- deref c
  store c $ v + 1
