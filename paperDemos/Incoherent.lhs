> {-# LANGUAGE TypeApplications #-}
> import ST
> import CSpec
> import Predicate

Not all protocols are internally coherent.
The protocol below is not satisfiable, it
is impossible to generate a number which is
bigger than a positive number and smaller than
a negative one.

> incoherent :: Spec Int Int
> incoherent = do
>   n1 <- send posNum
>   n2 <- send negNum
>   get $ inRange (n1, n2)

Using |coherent| we can check if a specification
is coherent or not

> main = coherent incoherent 

We get the following output

< ghci> main
< Failed with inability to generate: inRange (12,-3)
< In:
< ---
<   Sent: 12
<   Sent: -3

Note that we did not get a minimal counterexample, that's
because we use a timeout to detect when a predicate
is unsatisfiable. If we have a timeout of one second we
would have to spend an awful lot of time trying to shrink.
