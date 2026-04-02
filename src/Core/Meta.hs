module Core.Meta where

import Interface.MCU
import Ivory.Language

data Meta p = Meta
    { name :: String
    , model :: Uint8
    , version :: (Int, Int)
    , board :: Int
    , shouldInit :: IBool
    , mcu :: MCU p
    , quartzFrequency :: Int
    , systemFrequency :: Int
    }

mkName :: Meta p -> Maybe String -> String
mkName Meta{..} postfix =
    name
        <> maybe "" ("-" <>) postfix
        <> "-v_"
        <> show board
        <> "_"
        <> major version
        <> "_"
        <> minor version
        <> "-"
        <> mcu.model
        <> mcu.modification
  where
    major = show . fst
    minor = show . snd
