module Build.Shake  where

import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util


class Shake c where
    key   :: c -> String
    shake :: c -> [String] -> IO ()
