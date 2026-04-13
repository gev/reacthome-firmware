module Core.Meta where

import Data.List
import Data.Word
import Interface.MCU
import Ivory.Language

data Meta p = Meta
    { name :: String
    , model :: Word16
    , version :: (Word8, Word8)
    , board :: Word8
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

mkNameDfu :: Meta p -> (Word8, Word8) -> String
mkNameDfu Meta{..} dfuVersion =
    intercalate
        "-"
        [ name
        , show board
        , major version <> "_" <> minor version
        , major dfuVersion <> "_" <> minor dfuVersion
        , mcu.model <> mcu.modification
        ]
  where
    major = show . fst
    minor = show . snd
