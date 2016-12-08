{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import ST
import Typeclasses
import Predicate
import Foreign.Erlang
import Control.DeepSeq
import Test.QuickCheck


buyer'' :: Double -> ST ErlType
buyer'' price = Send (inRange (0, price+0.5)) $ \reqP ->
                ("response", Get (inRange (0, price+0.5)) $ \bp ->
                    if bp <= reqP then
                        End
                    else
                        buyer'' bp
                ) <&> ("fault", End)

buyer2 :: ST ErlType
buyer2 = buyer'' 100

{- The process of iterating a specification, to show that "checkCoherence" can be useful -}
type Message = Int

validMessage :: Predicate Message
validMessage = posNum

-- Send any number of messages
protocol :: ST Int
protocol = Send validMessage .- ("another", protocol) <|> ("finish", End)

-- Tell the other party how many messages will be sent
protocol2 :: ST Int
protocol2 = Send (posNum :: Predicate Int) .- protocol

-- Send precisely the number of messages that you said you would
protocol3 :: ST Int
protocol3 = Send posNum $ \n -> foldr (.-) End (replicate n (Send validMessage))

-- Messages of a certain size
type Message' = (Int, Message)

validMessageInSizeRange :: (Int, Int) -> Predicate Message'
validMessageInSizeRange r = inRange r .*. validMessage

-- This specification is WRONG on purpose!
protocol4 :: ST ErlType
protocol4 = Send posNum $ \n -> Send (validMessageInSizeRange (1, n)) (messages n)

messages :: Int -> Message' -> ST ErlType
messages 0 _ = End
messages n m = Send (validMessageInSizeRange (1, n - (fst m))) (messages (n - (fst m)))

-- Fixing the bug in protocol4
protocol5 :: ST ErlType
protocol5 = Send posNum $ \n -> Send (validMessageInSizeRange (1, n)) (messages' n)

messages' :: Int -> Message' -> ST ErlType
messages' 0 _ = End
messages' n m
    | n - (fst m) == 0 = End
    | otherwise        = Send (validMessageInSizeRange (1, n - (fst m))) (messages' (n - (fst m)))

{- The TCP handshake -}
newtype SYN    = SYN Int           deriving (Show, Eq, NFData, Arbitrary)
newtype SYNACK = SYNACK (Int, Int) deriving (Show, Eq, NFData, Arbitrary)
newtype ACK    = ACK (Int, Int)    deriving (Show, Eq, NFData, Arbitrary)
newtype TCPIntMessage = TCPINT (Int, Int) deriving (Show, Eq, NFData, Arbitrary)

data TCP = Syn SYN | SynAck SYNACK | Ack ACK | TCPINTM TCPIntMessage deriving (Show)

instance SYN :<: TCP where
    embed x = Syn x
    extract (Syn x) = Just x
    extract _       = Nothing

instance SYNACK :<: TCP where
    embed x = SynAck x
    extract (SynAck x) = Just x
    extract _       = Nothing

instance ACK :<: TCP where
    embed x = Ack x
    extract (Ack x) = Just x
    extract _       = Nothing

instance TCPIntMessage :<: TCP where
    embed x = TCPINTM x
    extract (TCPINTM x) = Just x
    extract _       = Nothing

-- This is a great example of where we fail at writing specifications!
handshake :: (ACK -> ST TCP) -> ST TCP
handshake cont = Send wildcard $ \(SYN x) -> Get (synack x) $ \(SYNACK (x', y)) -> Send (is (ACK (x'+1, y+1))) cont
    where
        synack x = predicate
            ("synack " ++ show x)
            (
            (do
                y <- arbitrary
                return $ SYNACK (x+1, y)
            ),
            (\(SYNACK (x', _)) -> x' == x + 1)
            )

-- simplified RFC 868
time :: ST TCP
time = handshake $ \(ACK (x, y)) -> Get (tcpMessage x posNum) .- End

tcpMessage :: Int -> Predicate Int -> Predicate TCPIntMessage 
tcpMessage x prd = predicate ("tcpMessage " ++ show x ++ " " ++ name prd)
                   (
                    (do
                        pn <- generator prd
                        return $ TCPINT (x+4, pn)
                    ),
                    (\(TCPINT (x', m)) -> x+4 == x' && (predf prd m) == Nothing))
