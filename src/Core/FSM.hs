{-# LANGUAGE DataKinds #-}
module Core.FSM where
import           Ivory.Language
import           Ivory.Stdlib



transit :: a -> b -> (a, b)
transit = (,)

(|->) = transit



runState :: (IvoryStore a, IvoryVar (ref s (Stored a)), IvoryRef ref, IvoryEq a)
         => (t -> ref s (Stored a))
         -> [(a, t -> p -> Ivory eff ())]
         -> t
         -> p
         -> Ivory eff ()
runState f ts st i = do
    p <- deref (f st)
    let run (w, h) = w ==? p ==> h st i
    cond_ $ run <$> ts



runInput :: IvoryEq p
         => d
         -> [(d -> p, t -> p -> Ivory eff ())]
         -> t
         -> p
         -> Ivory eff ()
runInput d ts st i =
    cond_ $ run <$> ts
    where run (f, h) = f d ==? i ==> h st i
