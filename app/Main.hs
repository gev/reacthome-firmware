module Main where

import Build.Compiler.GCC
import Build.Make
import Formula.AO4
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
import Formula.MixV
import Formula.Relay12
import Formula.RoomNumber
import Formula.RsHub4
import Formula.Server
import Formula.Smart.Bottom.Bottom
import Formula.Smart.Bottom.BottomCO2
import Formula.Smart.Bottom.BottomClimate
import Formula.Smart.Top.TopA4P
import Formula.Smart.Top.TopA4T
import Formula.Smart.Top.TopA4TD
import Formula.Smart.Top.TopA6P
import Formula.Smart.Top.TopA6T
import Formula.Smart.Top.TopCardHolder
import Formula.Smart.Top.TopClimate
import Formula.Smart.Top.TopG2
import Formula.Smart.Top.TopG4
import Formula.Smart.Top.TopG4D
import Formula.Smart.Top.TopG6
import Formula.Soundbox
import Formula.MixF

main :: IO ()
main = do
    mapM_
        (make gcc)
        [ leakSensor'v3
        ]
    mapM_
        (make gcc)
        [ ao4'v7
        , di4'v3
        , di4la'v3
        , di4rsm'v7
        , relay12'v6
        , dimmerDC12'v6
        , dimmerAC12'v6
        , dimmerAC1'v3
        , doppler1'v5
        , doppler5'v5
        , mix6x12'v6
        , mixF'v2
        , mixH'v3
        , mixH'v6
        , roomNumber'v1
        , smartBottom'v13
        , smartBottomCO2'v13
        , smartBottomClimate'v13
        , smartTopA4P'v6
        , smartTopA4T'v3
        , smartTopA4T'v5
        , smartTopA4TD'v3
        , smartTopA4TD'v5
        , smartTopA6P'v6
        , smartTopA6T'3
        , smartTopA6T'v5
        , smartTopCardHolder'v1
        , smartTopClimate'v0
        , smartTopG2'v2
        , smartTopG4'v2
        , smartTopG4D'v13
        , smartTopG4D'v15
        , smartTopG2'v9
        , smartTopG4'v9
        , smartTopG6'v2
        , smartTopG6'v9
        ]
    mapM_
        (make gcc)
        [ mixV'v2
        , rsHub4'v1
        , server'v11
        , soundbox'v5
        ]
