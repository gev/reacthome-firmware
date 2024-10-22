module Main where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Firmware
import           Control.Monad.State
import           Device.GD32F3x0
import           Device.GD32F4xx
import           Formula.Blink330             (blink330)
import           Formula.DI4
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
    -- gcc gd32f350k8u6 [ di4rsm
    --                  ]

    -- gcc gd32f330k8u6 [ relay12
    --                  , dimmerDC12
    --                  , dimmerAC12
    --                  , mix6x12
    --                  , di4
    --                  , smartBottom1
    --                  , smartBottom2
    --                  , smartTopA4P
    --                  , smartTopG6
    --                  , smartTopG4
    --                  , smartTopG2
    --                  , smartTopA6P
    --                  , smartTopA4T
    --                  , smartTopA6T
    --                  , smartTopG4D
    --                  , doppler1
    --                  , doppler5
    --                  ]

    gcc gd32f330k8u6 [ doppler5 ]

    gcc gd32f450vgt6 [ server
                     , rsHub4
                     ]
