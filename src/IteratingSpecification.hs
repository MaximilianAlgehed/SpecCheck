{-# LANGUAGE TypeOperators, RankNTypes #-}
import CSpec
import Typeclasses
import ST
import Predicate
import Foreign.Erlang
import Control.Monad

type Message = Int

validMessage :: Predicate Message
validMessage = posNum

-- Send any number of messages
protocol :: CSpec ErlType ()
protocol =
    do
        send validMessage
        choice <- choose ["another", "finish"]
        case choice of
            "another" -> protocol
            "finish"  -> stop

-- Tell the other party how many messages will be sent
protocol2 :: CSpec ErlType ()
protocol2 =
    do
        send posNum :: CSpec ErlType Int
        protocol

-- Send precisely the number of messages that you said you would
protocol3 :: CSpec ErlType ()
protocol3 =
    do
        n <- send posNum
        void $ replicateM n (send validMessage)

-- Messages of a certain size
type Message' = (Int, Message)

validMessageInSizeRange :: (Int, Int) -> Predicate Message'
validMessageInSizeRange r = inRange r .*. validMessage

protocol4 :: CSpec ErlType ()
protocol4 =
    do
        n <- send posNum
        messages n

messages :: Int -> CSpec ErlType ()
messages n =
    do
        m <- send $ validMessageInSizeRange (1, n)
        messages  $ n - (fst m)
