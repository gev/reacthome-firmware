{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Build.Compiler where

import           Build.Firmware
import           Build.Shake
import           Core.Context
import           Core.Formula
import           Interface.MCU

class Shake c => Compiler c p where
    makeConfig :: MCU p -> c

    build :: [(Formula p, String)] -> MCU p -> Context -> c -> IO ()
    build ms mcu context config = do
        mapM_ run ms
        shake config $ snd <$> ms
        where
            run (f@(Formula {..}), name) = do
                compile (cook f mcu context, name)
