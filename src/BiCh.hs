{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             DeriveFunctor #-}
-- | This file gives a model of communication between two actors implementing a session
-- | type
module BiCh where
import Typeclasses
import Foreign.Erlang
import Control.Concurrent.Chan

-- | Protocol
data Protocol t = Pure t
                | Choice String 
                deriving (Show,Functor)

data Interaction t = Got t | Sent t deriving (Show, Functor)
type Log t = [Interaction (Protocol t)]

-- | BiChannel interaction
data P a x = P (a x) (a x) (a ()) (a ())

class BiChannel ch t where
    new :: IO (ch (Protocol t))
    put :: ch (Protocol t) -> Protocol t -> IO ()
    get :: ch (Protocol t) -> IO (Protocol t)
    bidirect :: ch (Protocol t) -> ch (Protocol t)
    kill :: ch (Protocol t) -> IO ()
    killAcc :: ch (Protocol t) -> IO ()
    waitToBeKilled :: ch (Protocol t) -> IO ()

instance BiChannel (P Chan) a where
    new = do
            cin <- newChan
            cout <- newChan
            exit_chan <- newChan
            eca <- newChan
            return $ P cin cout exit_chan eca
    put (P _ cout _ _) = writeChan cout
    get (P cin _ _ _) = readChan cin
    bidirect (P cin cout exit_chan eca) = P cout cin exit_chan eca
    kill (P _ _ exit_chan eca) = (writeChan exit_chan ()) >> readChan eca
    waitToBeKilled (P _ _ exit_chan _) = readChan exit_chan
    killAcc (P _ _ _ eca) = writeChan eca ()

instance (t :<: ErlType) => Protocol t :<: ErlType where
    embed (Pure t)    = ErlTuple [ErlAtom "pure", embed t]
    embed (Choice s)  = ErlTuple [ErlAtom "choice", ErlAtom s]

    extract (ErlTuple [ErlAtom "pure", t])           = fmap Pure $ extract t
    extract (ErlTuple [ErlAtom "choice", ErlAtom s]) = return $ Choice s
    extract _ = Nothing
