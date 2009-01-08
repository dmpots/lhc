-- demonstrates bug in interaction between multi-parameter newtypes
-- and class instances rules? (Please setup a bug tracker soon! Then I
-- could just refer to the bug number, and not write an unclear/false
-- description of the bug ;-)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

class Arrow a where
    arr :: (b -> c) -> a b c

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)

main :: IO ()
main = runKleisli (arr id) ()
