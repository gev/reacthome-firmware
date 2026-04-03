module Core.Meta where

import Data.List
import Interface.MCU
import Ivory.Language

data Meta p = Meta
    { name :: String
    , model :: Int
    , version :: (Int, Int)
    , board :: Int
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    }

mkName :: Meta p -> String
mkName Meta{..} =
    intercalate
        "-"
        [ name
        , show board
        , major version <> "_" <> minor version
        , mcu.model <> mcu.modification
        ]
  where
    major = show . fst
    minor = show . snd
