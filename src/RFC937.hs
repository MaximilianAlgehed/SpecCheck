{-# LANGUAGE RankNTypes, TypeOperators, MultiParamTypeClasses, DeriveAnyClass #-}
import Typeclasses
import Test.QuickCheck hiding (choose)
import ST
import Foreign.Erlang
import Predicate
import CSpec
import qualified Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.List
import Control.DeepSeq
import Data.Generics

heloMessage = is ("HELO", "apa", "bepa") ||| wildcard

loginStatus = predicate "loginStatus" (fmap (fmap abs) arbitrary, check)
  where
    check (Just x) = x >= 0
    check _        = True

login :: CSpec ErlType Int
login =
  do
    send heloMessage
    status <- get loginStatus
    case status of
      Nothing  -> stop
      Just x -> return x 
