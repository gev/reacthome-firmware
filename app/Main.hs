module Main where

import           Build.Compiler.GCC
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.DI4
import           Formula.DI4LA
import           Formula.DI4RSM
import           Formula.DimmerAC12
import           Formula.DimmerDC12
import           Formula.Doppler1
import           Formula.Doppler5
import           Formula.Mix6x12
import           Formula.Relay12
import           Formula.RsHub4
import           Formula.Server
import           Formula.Smart.Bottom.Bottom1
import           Formula.Smart.Bottom.Bottom2
import           Formula.Smart.Top.TopA4P
import           Formula.Smart.Top.TopA4T
import           Formula.Smart.Top.TopA6P
import           Formula.Smart.Top.TopA6T
import           Formula.Smart.Top.TopG2
import           Formula.Smart.Top.TopG4
import           Formula.Smart.Top.TopG4D
import           Formula.Smart.Top.TopG6


main :: IO ()
main = do
    mapM_ gcc [ di4
              , di4la
              , di4rsm
              , relay12
              , dimmerDC12
              , dimmerAC12
              , doppler1
              , doppler5
              , mix6x12
              , smartBottom1
              , smartBottom2
              , smartTopA4P
              , smartTopA4T
              , smartTopA6P
              , smartTopA6T
              , smartTopG2
              , smartTopG4
              , smartTopG4D
              , smartTopG6
              ]
    mapM_ gcc [ rsHub4
              , server
              ]
