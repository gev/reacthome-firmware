module Main where

import Build.Compiler.GCC
import Formula.DI4
import Formula.DI4LA
import Formula.DI4RSM
import Formula.DimmerAC1
import Formula.DimmerAC12
import Formula.DimmerDC12
import Formula.Doppler1
import Formula.Doppler5
import Formula.LeakSensor
import Formula.Mix6x12
import Formula.MixH
import Formula.Relay12
import Formula.RsHub4
import Formula.Server
import Formula.Smart.Bottom.Bottom1
import Formula.Smart.Bottom.Bottom2
import Formula.Smart.Top.TopA4P
import Formula.Smart.Top.TopA4T
import Formula.Smart.Top.TopA4TD
import Formula.Smart.Top.TopA6P
import Formula.Smart.Top.TopA6T
import Formula.Smart.Top.TopG2
import Formula.Smart.Top.TopG4
import Formula.Smart.Top.TopG4D
import Formula.Smart.Top.TopG6
import Formula.Smart.Top.TopG6I
import Formula.Smart.Top.TopG6IT
import Formula.Smart.Top.TopG6Test
import Formula.Soundbox

main :: IO ()
main = do
    mapM_
        gcc
        [ di4
        , di4la
        , di4rsm
        , relay12
        , dimmerDC12
        , dimmerAC12
        , dimmerAC1
        , doppler1
        , doppler5
        , leakSensor
        , mix6x12
        , mixH
        , smartBottom1
        , smartBottom2
        , smartTopA4P
        , smartTopA4T
        , smartTopA4TD
        , smartTopA6P
        , smartTopA6T
        , smartTopG2
        , smartTopG4
        , smartTopG4D
        , smartTopG6
        , smartTopG6I
        , smartTopG6IT
        , smartTopG6Test
        ]
    mapM_
        gcc
        [ rsHub4
        , server
        , soundbox
        ]
