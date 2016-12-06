{-# LANGUAGE TypeOperators #-}
module CSpec where
import Control.Monad.Cont
import ST
import Predicate
import Typeclasses
import Test.QuickCheck
import Control.DeepSeq

send :: (a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> CSpec t a
send = cont . Send

get :: (a :<: t, Show a, Arbitrary a, NFData a) => Predicate a -> CSpec t a
get = cont . Get

stop :: CSpec t a
stop = cont (const End)

choose :: (a :<: t, Show a, Arbitrary a, NFData a, Eq a) => [a] -> CSpec t a
choose = send . from
