> {-# LANGUAGE TypeApplications, TypeOperators, FlexibleContexts #-}
> module Iterating where
> import ST
> import CSpec
> import Predicate
> import Typeclasses

Imagine a programmer writing a protocol which looks something
like the following

> protocol_v0 :: (Int :<: t, String :<: t) => Spec t Int
> protocol_v0 = do
>   send validMessage
>   choice <- choose ["more", "stop"]
>   case choice of
>     "more" -> protocol_v0
>     "stop" -> stop

> validMessage = anything @Int

There is nothing wrong with this specification.
However, the programmer notices that her implementation of the protocol is running slowly on the receiver due to a new
buffer having to be allocated for each new message. She decides to optimize the implementation by first
sending an integer representing the number of messages that will be sent.

> protocol_v1 :: (Int :<: t, String :<: t) => Spec t Int
> protocol_v1 = do
>   send (posNum @Int)
>   protocol_v0

Upon generating some plausible communication traces from this specification the programmer notices a problem

< ghci> exampleOf protocol_v1
< sent 10
< sent 1
< sent "stop"
< stop

Our programmer recognises that the specification of |protocol_v1| is quite problematic, it does not enforce that
the number of sent messages is actually equal to the number sent at the start of the interaction.
However, the programmer in question is a clever functional programmer with knowledge of fantastic things like
|fold| and |>>|!

> protocol_v2 :: (Int :<: t) => Spec t Int
> protocol_v2 = do
>   n <- send posNum
>   foldr (>>) stop $ replicate n (send validMessage)

As a sanity check our programmer checks that this version of the protocol is coherent

< ghci> coherent protocol_v2
< passed

She also generates some examples of possible communications, inlcuding the following

< ghci> exampleOf protocol_v2
< sent 5
< sent 0
< sent 1
< sent 10
< sent 12
< sent 3
< stop

Satisfied with the result she proceedes to implement the sending process in Python and wants to test it
using SpecCheck. She does so using the |testTCP| function

< ghci> testTCP "implementation.py" (dual protocol_v2)
< Failed after 0 tests
< With: Timeout
< In:
< ---
<   sent 3
<   sent 1
<   sent -5
< ---
< ~~~~ After shrinking
< ---
<   sent 1
< ---

It is immidiately clear that the implementation suffers from an "off-by-one" error (common when programming in
filthy imperative languages)!
