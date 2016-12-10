{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import ST
import Typeclasses
import Predicate
import Foreign.Erlang
import Control.DeepSeq
import Test.QuickCheck

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
