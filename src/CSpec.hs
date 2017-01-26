{-# LANGUAGE TypeOperators,
             GADTs,
             Rank2Types #-}
module CSpec where
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Predicate
import Typeclasses
import Test.QuickCheck
import Control.DeepSeq
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State as S

-- It would be good if we could hide the IO bit (or would it?)
data ST m c where
  Send   :: (Monad (m IO), Arbitrary a, Show a, a :<: c, NFData a) => Predicate a -> (a -> m IO (ST m c)) -> ST m c
  Get    :: (Monad (m IO), Arbitrary a, Show a, a :<: c, NFData a) => Predicate a -> (a -> m IO (ST m c)) -> ST m c 
  Nop    :: (() -> m IO (ST m c)) -> ST m c
  End    :: ST m c

type Spec t r     = forall m. (Monad (m IO), MonadTrans m) => SpecT m t r
type SpecS st t r = SpecT (S.StateT st) t r
type SpecT m t r  = ReaderT [String] (ContT  (ST m t) (m IO)) r

send :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> SpecT m t a
send p = lift $ ContT $ fmap return (Send p)

get :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> SpecT m t a
get p = lift $ ContT $ fmap return (Get p)

stop :: (MonadTrans m, Monad (m IO))  => SpecT m t a
stop = lift $ ContT $ fmap return (const End)

nop :: (MonadTrans m, Monad (m IO)) => SpecT m t ()
nop = lift $ ContT $ fmap return Nop

-- Declare what to do in the case of a bug
bug :: String -> SpecT m t r -> SpecT m t r -> SpecT m t r
bug b nonBuggy buggy = do
  bugs <- ask
  if b `elem` bugs then
    buggy
  else
    nonBuggy

choose :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a, Eq a) => [a] -> SpecT m t a
choose = send . from

chooseFreq :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a, Eq a) => [(Int, a)] -> SpecT m t a
chooseFreq = send . fromFreq

branch :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a, Eq a) => [a] -> SpecT m t a
branch = get . from

state :: SpecS st t st
state = nop >> (lift $ lift S.get)

modify :: (st -> st) -> SpecS st t ()
modify fun = nop >> (lift $ lift $ S.modify fun)

store :: st -> SpecS st t ()
store st = nop >> (lift $ lift $ S.put st)

dual :: (Functor (m IO)) => SpecT m t a -> SpecT m t a
dual = mapReaderT (mapContT (fmap dualST))

dualST :: (Functor (m IO)) => ST m a -> ST m a
dualST (Send pred cont) = Get pred  $ \a -> fmap dualST (cont a)
dualST (Get pred cont)  = Send pred $ \a -> fmap dualST (cont a)
dualST (Nop f)          = Nop $ \() -> fmap dualST (f ())
dualST End              = End
