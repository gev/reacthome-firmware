module Core.Meta where

import Data.List
import Data.Word
import Interface.Flash
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
    , startEtc :: Maybe Int
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

mkEtc :: Meta p -> Flash p
mkEtc meta =
    maybe
        (error "Unknown etc start address")
        (mkOffset meta.mcu.flash)
        meta.startEtc