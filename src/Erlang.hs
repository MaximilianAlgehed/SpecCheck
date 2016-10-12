module Erlang where
import Control.Concurrent
import Control.Concurrent.Chan
import System.Process
import Model
import Foreign.Erlang
import Debug.Trace
import Run
import Typeclasses
import SessionTypes
import qualified LTL as LTL

-- | So that we can talk to Erlang!
instance (Erlang t) => Erlang (Protocol t) where
    toErlang (Pure t)    = ErlTuple [ErlAtom "pure", toErlang t]
    toErlang ChooseLeft  = ErlAtom "chooseLeft"
    toErlang ChooseRight = ErlAtom "chooseRight"

    fromErlang (ErlTuple [ErlAtom "pure", t]) = Pure (fromErlang t)
    fromErlang (ErlAtom "chooseLeft")         = ChooseLeft
    fromErlang (ErlAtom "chooseRight")        = ChooseRight
    fromErlang x                              = trace (show x) undefined

runErlang :: (Erlang r, Implements r t, Show r, Checks t r)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> SessionType t -- The session type for the interaction
          -> LTL.LTL (Interaction (Protocol r)) -- The LTL predicate
          -> IO ()
runErlang self mod fun st ltl = quickTest (runfun self) st ltl
    where
        runfun :: (Erlang r) => Self -> P Chan (Protocol r) -> IO ()
        runfun self ch =
            do
                mbox <- createMBox self
                rpcCall mbox (Short "erl") mod fun []
                id1 <- forkIO $ erlangLoop ch mbox
                id2 <- forkIO $ haskellLoop ch mbox
                return ()

        finish pid id1 id2 =
            do
                killThread id1
                killThread id2

        erlangLoop ch mbox =
            do
               m <- mboxRecv mbox
               put ch $ fromErlang m
               erlangLoop ch mbox
        
        haskellLoop ch mbox =
            do
                m <- get ch
                mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox, m)
                haskellLoop ch mbox
