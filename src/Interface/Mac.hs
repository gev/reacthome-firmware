{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Interface.Mac (Mac, mac, getMac) where

import           Core.Context
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


data Mac = Mac
    { name    :: String
    , initMac :: Buffer 6 Uint8 -> forall eff. Ivory eff ()
    , getMac  :: Buffer 6 Uint8
    }


mac :: (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> String
    -> Mac
mac init name = Mac
    { name    = name
    , initMac = init
    , getMac  = buffer name
    }



instance Include Mac where
    include (Mac {..}) = do
        include getMac
        include initMac'
        where
            initMac' :: Def ('[] ':-> ())
            initMac' = proc (name <> "_init") $ body $ initMac getMac
