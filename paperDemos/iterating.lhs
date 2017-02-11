> {-# LANGUAGE TypeApplications, TypeOperators, FlexibleContexts #-}
> import ST
> import CSpec
> import Predicate
> import Typeclasses

> validMessage = anything @Int

> protocol_v0 :: (Int :<: t, String :<: t) => Spec t Int
> protocol_v0 = do
>   send validMessage
>   choice <- choose ["more", "stop"]
>   case choice of
>     "more" -> protocol_v0
>     "stop" -> stop

There is nothing wrong with this specification.
However, the programmer notices that their implementation of the protocol is running slowly on the receiver due to a new
buffer having to be allocated for each new message. He or she decides to optimize the implementation by first
sending an integer representing the number of messages that will be sent.

> protocol_v1 :: (Int :<: t, String :<: t) => Spec t Int
> protocol_v1 = do
>   send validMessage
>   protocol_v0

Upon generating some plausible communication traces from this specification the programmer notices a problem

< ghci> examplesOf protocol_v1
< sent 10
< sent 1
< sent "stop"

Our programmer recognises that the specification of |protocol_v1| is quite problematic, it does not enforce that
the number of sent messages is actually equal to the number sent at the start of the interaction.
However, the programmer in question is a clever functional programmer with knowledge of fantastic things like
|fold| and |>>|!

> protocol_v2 :: (Int :<: t) => Spec t Int
> protocol_v2 = do
>   n <- send (anything @Int)
>   foldr (>>) stop $ replicate n (send validMessage)

As a sanity check our programmer checks that this version of the protocol is coherent

< ghci> coherent protocol_v2
< passed

Fantastic, the specification is complete!

> protocol_v3 :: (Int :<: t) => Spec t Int
> protocol_v3 = do
>   n <- send (posNum @Int)
>   foldr (>>) stop $ replicate n (send validMessage)
