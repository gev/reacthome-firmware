module Core.Meta where

import Interface.MCU
import Ivory.Language

data Meta p = Meta
    { name :: String
    , model :: Uint8
    , version :: (Int, Int)
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    }

mkName :: Meta p -> Maybe String -> String
mkName Meta{..} postfix =
    name
        <> maybe "" ("-" <>) postfix
        <> "-"
        <> mcu.model
        <> mcu.modification
        <> "-"
        <> major version
        <> "."
        <> minor version
  where
    major = show . fst
    minor = show . snd
