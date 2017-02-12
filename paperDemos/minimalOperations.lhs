> {-# LANGUAGE GADTs, RankNTypes, TypeOperators #-}
> import Typeclasses
> import Predicate
> import Control.Monad
> import Test.QuickCheck

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
