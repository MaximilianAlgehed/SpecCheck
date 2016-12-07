{-# LANGUAGE TypeOperators,
             GADTs,
             KindSignatures #-}
module CSpec where
import Control.Monad.Cont
import Predicate
import Typeclasses
import Test.QuickCheck
import Control.DeepSeq
import Control.Monad.Trans.Identity

-- It would be good if we could hide the IO bit (or would it?)
data ST (m :: (* -> *) -> * -> *) c where
    Send   :: (MonadTrans m, Arbitrary a, Show a, a :<: c, NFData a) => Predicate a -> (a -> m IO (ST m c)) -> ST m c
    Get    :: (MonadTrans m, Arbitrary a, Show a, a :<: c, NFData a) => Predicate a -> (a -> m IO (ST m c)) -> ST m c 
    End    :: ST m c

type CSpec t = CSpecT IdentityT t
type CSpecT m t = ContT (ST m t) (m IO)

send :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> CSpecT m t a
send p = ContT $ fmap return (Send p)

get :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> CSpecT m t a
get p = ContT $ fmap return (Get p)

stop :: (MonadTrans m, Monad (m IO))  => CSpecT m t a
stop = ContT $ fmap return (const End)

choose :: (MonadTrans m, Monad (m IO), a :<: t, Show a, Arbitrary a, NFData a, Eq a) => [a] -> CSpecT m t a
choose = send . from
