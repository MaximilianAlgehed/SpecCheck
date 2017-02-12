The minimal operations which are required in order to express the kinds of
protocols in "incoherent.hs", "iterating.lhs", and "bookShop.hs" are the following

> send   :: (a :<: t) => Predicate a -> Spec t a
> get    :: (a :<: t) => Precicate a -> Spec t a
> state  :: SpecS st t st
> modify :: (st -> st) -> SpecS st t ()
> stop   :: Spec t ()

Where |Spec = forall st. SpecS st| and |SpecS st t| is a monad which fulfills
at least the following

< stop     >>= f     = stop
< modify f >>= state = state >>= \st -> modify f >>= return (f a)

We also need to support the |dual| operation, with the following specification

< dual (send p)   = get p
< dual (get p)    = send p
< dual (m >>= f)  = dual m >>= (dual . f)
< dual (return a) = return a
< dual state      = state 
< dual (modify f) = modify f

From this we can derive several operations like

> put    :: st -> SpecS st t ()
> choose :: (Eq a, a :<: t) => [a] -> Spec t a
> branch :: (Eq a, a :<: t) => [a] -> Spec t a
