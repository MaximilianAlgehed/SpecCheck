{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, FlexibleContexts, FlexibleInstances #-}
-- | This file gives a model of communication between two actors implementing a session
-- | type
module BiCh where
import Control.Concurrent.Chan

-- | Protocol
data Protocol t = Pure t
                | ChooseLeft
                | ChooseRight
                deriving (Show,Functor)

data Interaction t = Got t | Sent t deriving (Show, Functor)
type Log t = [Interaction (Protocol t)]

-- | BiChannel interaction
data P a x = P (a x) (a x) (a ())

class BiChannel ch t where
    new :: IO (ch (Protocol t))
    put :: ch (Protocol t) -> Protocol t -> IO ()
    get :: ch (Protocol t) -> IO (Protocol t)
    bidirect :: ch (Protocol t) -> ch (Protocol t)
    kill :: ch (Protocol t) -> IO ()
    waitToBeKilled :: ch (Protocol t) -> IO ()

instance BiChannel (P Chan) a where
    new = do
            cin <- newChan
            cout <- newChan
            exit_chan <- newChan
            return $ P cin cout exit_chan
    put (P _ cout _) = writeChan cout
    get (P cin _ _) = readChan cin
    bidirect (P cin cout exit_chan) = P cout cin exit_chan
    kill (P _ _ exit_chan) = writeChan exit_chan ()
    waitToBeKilled (P _ _ exit_chan) = readChan exit_chan
