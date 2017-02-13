> {-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
> import Typeclasses
> import Predicate

> import Test.QuickCheck

> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Trans.Either
> import qualified Control.Monad.Trans.State as S
> import Control.Concurrent

The minimal operations which are required in order to express the kinds of
protocols in "incoherent.hs", "iterating.lhs", and "bookShop.hs" are the following

< send   :: (a :<: t) => Predicate a -> Spec t a
< get    :: (a :<: t) => Predicate a -> Spec t a
< state  :: SpecS st t st
< modify :: (st -> st) -> SpecS st t ()
< stop   :: Spec t ()

Where we have two functions

< generator :: Predicate a -> Gen a
< ($$) :: Predicate a -> a -> Bool

We can also define |Spec t a| as

> type Spec t a = forall st. SpecS st t a

We also need to support the |dual| operation, with the following specification

< dual (send p)   = get p
< dual (get p)    = send p
< dual (m >>= f)  = dual m >>= (dual . f)
< dual (return a) = return a
< dual state      = state 
< dual (modify f) = modify f

From this we can derive several operations like

< put    :: st -> SpecS st t ()
< choose :: (Eq a, a :<: t) => [a] -> Spec t a
< branch :: (Eq a, a :<: t) => [a] -> Spec t a

We can implement this as a GADT

> data SpecS st t a where
>   Send   :: (a :<: t) => Predicate a -> SpecS st t a
>   Get    :: (a :<: t) => Predicate a -> SpecS st t a
>   State  :: SpecS st t st
>   Modify :: (st -> st) -> SpecS st t ()
>   Stop   :: SpecS st t a
>   Return :: a -> SpecS st t a
>   (:>>=) :: SpecS st t a -> (a -> SpecS st t b) -> SpecS st t b

From which we get the monad instance and our operations for free

> instance Monad (SpecS st t) where
>   return = Return
>   (>>=)  = (:>>=)

> send :: (a :<: t) => Predicate a -> SpecS st t a
> send = Send

> get :: (a :<: t) => Predicate a -> SpecS st t a
> get = Get

> state :: SpecS st t st
> state = State

> modify :: (st -> st) -> SpecS st t ()
> modify = Modify

And, of course, we are forced to give default instances for |Applicative| and |Functor|

> instance Applicative (SpecS st t) where
>   pure  = return
>   (<*>) = ap

> instance Functor (SpecS st t) where
>   fmap  = (<$>)

We can now, trivially, derive the implementation of |dual| from the specification

> dual :: SpecS st t a -> SpecS st t a
> dual State      = State
> dual (Modify f) = Modify f
> dual (Return a) = Return a
> dual Stop       = Stop
> dual (Send p)   = Get p
> dual (Get p)    = Send p
> dual (m :>>= f) = dual m :>>= (dual . f)

As we are interested in using our specifications for testing applications which communicate
using a bi-directional, full duplex, channel we make use of the |Chan| type [1]  to
introduce the following type for bi-directional channels

> type BiChan a = (Chan a, Chan a)

> newBiChan :: IO (BiChan a)
> newBiChan = (,) <$> newChan <*> newChan

> swapBiChan :: BiChan a -> BiChan a
> swapBiChan (read, write) = (write, read)

> readBiChan :: BiChan a -> IO a
> readBiChan = readChan . fst

> writeBiChan :: BiChan a -> a -> IO ()
> writeBiChan = writeChan . snd

Now that we have the plumbing in place we are ready to define an interpretation of our specifications
as |IO| operations that act as a communicating party

> execute :: BiChan t -> SpecS st t a -> S.StateT st (EitherT Bool IO) a
> execute _ State       = S.get
> execute _ (Modify f)  = S.modify f
> execute _ (Return a)  = return a
> execute _ Stop        = lift $ left True
> execute ch (Send p)   = do
>   a <- liftIO $ generate (generator p)
>   liftIO $ writeBiChan ch (embed a)
>   return a
> execute ch (Get p)    = do
>   a <- liftIO $ readBiChan ch
>   case extract a of
>     Nothing -> lift $ left False
>     Just a  -> if p $$ a then
>                  return a
>                else
>                  lift $ left False 
> execute ch (m :>>= f) = execute ch m >>= (execute ch) . f

References:
[1] "Concurrent Haskell", S. Peyton Jones et. al, 1996
