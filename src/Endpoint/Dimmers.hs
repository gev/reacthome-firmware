{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.Dimmers where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib


type DimmerStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { mode       :: Uint8
    ; brightness :: Uint8
    ; velocity   :: Uint8
    ; group      :: Uint8
    ; synced     :: IBool
    }
|]



data Dimmers = Dimmers
    { runDimmers :: RunRecords DimmerStruct
    , payload    :: Buffer 6 Uint8
    }

dimmers :: MonadWriter Context m => String -> Int -> m Dimmers
dimmers name n = do
    addStruct (Proxy :: Proxy DimmerStruct)
    let runDimmers = runRecords name $ replicate n go
    payload       <- buffer "dimmer_message"
    let dimmers    = Dimmers {runDimmers, payload}
    runDimmers addArea
    pure dimmers
    where go = [ mode       .= ival 0
               , brightness .= ival 0
               , velocity   .= ival 0
               , group      .= ival 1
               , synced     .= ival true
               ]



message :: Dimmers -> Uint8 -> Ivory eff (Buffer 6 Uint8)
message Dimmers{..} i = do
    runDimmers $ \d -> do
        let dimmer = addrOf d ! toIx i
        pack payload 0 (0xd0 :: Uint8)
        pack payload 1 $ i + 1
        pack payload 2 =<< deref (dimmer ~> group     )
        pack payload 3 =<< deref (dimmer ~> mode      )
        pack payload 4 =<< deref (dimmer ~> brightness)
        pack payload 5 =<< deref (dimmer ~> velocity  )
    pure payload



initialize :: Record DimmerStruct -> Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
initialize dimmer group' mode' brightness' velocity' = do
    store (dimmer ~> group     ) group'
    store (dimmer ~> mode      ) mode'
    store (dimmer ~> brightness) brightness'
    store (dimmer ~> velocity  ) velocity'

on :: Dimmers -> Uint8 -> Ivory eff ()
on = runCheckMode $ \dimmer ->
    store (dimmer ~> brightness) 255

off :: Dimmers -> Uint8 -> Ivory eff ()
off = runCheckMode $ \dimmer ->
    store (dimmer ~> brightness) 0

fade :: Uint8 -> Uint8 -> Dimmers -> Uint8 -> Ivory eff ()
fade brightness' velocity' = runCheckMode $ \dimmer -> do
    store (dimmer ~> brightness   ) brightness'
    store (dimmer ~> velocity) velocity'

setBrightness :: Uint8 -> Dimmers -> Uint8 -> Ivory eff ()
setBrightness brightness' = runCheckMode $ \dimmer -> do
    mode <- deref $ dimmer ~> mode
    ifte_ (mode ==? 4)
        (ifte_ (brightness' ==? 0)
                (store (dimmer ~> brightness ) 0)
                (store (dimmer ~> brightness ) 255)
        )
        (store (dimmer ~> brightness ) brightness')

setMode :: Uint8 -> Dimmers -> Uint8 -> Ivory eff ()
setMode  mode' = runDimmer $ \dimmer -> do
    store (dimmer ~> mode      ) mode'
    store (dimmer ~> brightness) 0
    store (dimmer ~> synced    ) false

setGroup :: Uint8 -> Dimmers -> Uint8 -> Ivory eff ()
setGroup group' = runDimmer $ \dimmer -> do
    store (dimmer ~> group ) group'
    store (dimmer ~> synced) false



runCheckMode :: (Record DimmerStruct -> Ivory eff ()) -> Dimmers -> Uint8 -> Ivory eff ()
runCheckMode run = runDimmer $ \dimmer -> do
    mode' <- deref $ dimmer ~> mode
    when (mode' /=? 0) $ do
        run dimmer
        store (dimmer ~> synced) false


runDimmer :: (Record DimmerStruct -> Ivory eff ()) -> Dimmers -> Uint8 -> Ivory eff ()
runDimmer run dimmers index =
    runDimmers dimmers $ \ds -> do
        let ix' = toIx index
        let dimmer' = addrOf ds ! ix'
        run dimmer'
        syncDimmerGroup ds dimmer' ix'



syncDimmerGroup :: KnownNat n => Records' n DimmerStruct -> Record DimmerStruct -> Ix n -> Ivory eff ()
syncDimmerGroup ds dimmer' ix' = do
    group' <- deref $ dimmer' ~> group
    arrayMap $ \ix'' ->
        when (ix'' /=? ix') $ do
            let dimmer'' = addrOf ds ! ix''
            group'' <- deref $ dimmer'' ~> group
            when (group'' ==? group') $ do
                let sync = copyLabel dimmer'' dimmer'
                sync velocity
                sync brightness
                sync mode
                store (dimmer'' ~> synced) false



copyLabel :: (IvoryStore a, IvoryStruct sym)
          => Ref s1 (Struct sym)
          -> Ref s2 (Struct sym)
          -> Label sym ('Stored a)
          -> Ivory eff ()
copyLabel dst src label = store (dst ~> label) =<< deref (src ~> label)
