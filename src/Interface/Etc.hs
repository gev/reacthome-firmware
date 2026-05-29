module Interface.Etc where

data Etc t = Etc
    { version :: Int
    , etc :: t
    }
