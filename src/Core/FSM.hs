{-# LANGUAGE DataKinds #-}
module Core.FSM where
import           Ivory.Language
import           Ivory.Stdlib



transit :: a -> b -> (a, b)
transit = (,)

(|->) = transit



runFSM :: (IvoryStore a, IvoryVar (ref s (Stored a)), IvoryRef ref, IvoryEq a)
           => (t -> ref s (Stored a))
           -> [(a, t -> p -> Ivory eff ())]
           -> t
           -> p
           -> Ivory eff ()
runFSM f ts st i = do
    p <- deref (f st)
    let go (w, h) = w ==? p ==> h st i
    cond_ $ go <$> ts
