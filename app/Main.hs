module Main where

import Build.Compiler.GCC
import Formula.DI4
import Formula.LeakSensor
import Formula.Smart.Top.TopG6I
import Formula.Smart.Top.TopG6IT

main :: IO ()
main = do
    mapM_
        gcc
        [ di4
        , -- , di4la
          -- , di4rsm
          -- , relay12
          -- , dimmerDC12
          -- , dimmerAC12
          -- , dimmerAC1
          -- , doppler1
          -- , doppler5
          -- , mix6x12
          -- , mixH
          -- , smartBottom1
          -- , smartBottom2
          -- , smartTopA4P
          -- , smartTopA4T
          -- , smartTopA4TD
          -- , smartTopA6P
          -- , smartTopA6T
          -- , smartTopG2
          -- , smartTopG4
          -- , smartTopG4D
          -- , smartTopG6
          smartTopG6I
        , smartTopG6IT
        , leakSensor
        -- , smartTopG6Test
        ]

-- mapM_ gcc [ rsHub4
--           -- , server
--           , soundbox
--           ]
