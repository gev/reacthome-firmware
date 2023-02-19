module Core.Transport where

import           Core.Task
import           Data.Buffer
import           Ivory.Language


class Task t => Transport t where
    transmit :: t -> Buffer l v -> n -> Ivory eff ()
