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
runFSM f hs r v = do
    p <- deref (f r)
    let go (w, h) = w ==? p ==> h r v
    cond_ $ go <$> hs
