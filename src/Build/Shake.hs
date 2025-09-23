module Build.Shake where

class Shake c where
    hash :: c -> String
    shake :: c -> String -> IO ()
