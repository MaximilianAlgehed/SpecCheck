{-
 - This file needs some major refactoring,
 - if you are afraid of ugly code, look away now.
 -
 - We should try to do something about the (a -> Maybe String)
 - thing, it must be possible to abstract this somehow.
 -}

{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             FlexibleContexts,
             KindSignatures,
             RankNTypes,
             UndecidableInstances,
             ScopedTypeVariables #-}
module ST where
import CSpec
import Control.Monad.Trans
import Control.DeepSeq
import Predicate
import System.Timeout
import System.Process
import Debug.Trace
import Prelude hiding (any)
import Test.QuickCheck
import Data.List hiding (any)
import Foreign.Erlang
import BiCh as BCH
import System.IO
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan
import Typeclasses
import Control.Monad.Cont
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State as S
import Control.Exception
import qualified Network.Simple.TCP as N
import qualified Data.ByteString.Char8 as BS
import Control.Exception

erlBool True = ErlAtom "true"
erlBool False = ErlAtom "false"

sessionTest :: (Show c, BiChannel ch c, MonadTrans m, Monad (m IO))
            => ST m c
            -> ch c
            -> WriterT (Log (c, String)) (m IO) (Maybe String)
sessionTest (Send p cont) ch =
    do
        value <- lift $ lift $ generate (generator p)
        lift $ lift $ put ch $ embed value
        tell [Sent (embed value, show value)]
        c <- lift $ cont value
        sessionTest c ch
sessionTest (Get p cont) ch =
    do
        mv <- lift $ lift $ BCH.get ch
        case extract mv of
            Just value -> if predf p value == Nothing then
                            do
                                tell [Got (mv, show value)]
                                c <- lift $ cont value
                                sessionTest c ch
                          else
                            do
                                tell [Got (mv, show value)]
                                return $ fmap (++" "++(show value)) (predf p value)
            Nothing    -> do 
                            tell [Got (mv, show mv)]
                            return $ Just "Type error!"
sessionTest End _ = return Nothing

data ShrinkStatus = FailedToShrink | FailingPredicate String

fromMaybeSS :: Maybe String -> ShrinkStatus
fromMaybeSS Nothing = FailedToShrink
fromMaybeSS (Just s) = FailingPredicate s

sessionShrink :: (Show c, BiChannel ch c, MonadTrans m, Monad (m IO))
            => Int
            -> Log c
            -> ST m c
            -> ch c 
            -> WriterT (Log (c, String)) (m IO) ShrinkStatus -- Fix the return type to be "Log (c, String)" instead
                                                         -- of "Log String"
sessionShrink 0 _ _   _ = return FailedToShrink -- Our trace is longer than the original trace
sessionShrink _ _ End _ = return FailedToShrink -- We didn't failsify the property
sessionShrink n (x:xs) st ch
    | traceMatch x st   = case (x, st) of
                            (Got _, Get p cont) ->
                                do
                                    mv <- lift $ lift $ BCH.get ch
                                    case extract mv of
                                        Just value -> if predf p value == Nothing then
                                                        do
                                                            tell [Got (mv, (show value))]
                                                            c <- lift $ cont value
                                                            sessionShrink (n-1) xs c ch
                                                      else
                                                        do
                                                            tell [Got (mv, (show value))]
                                                            return $ fromMaybeSS $ fmap (++" "++(show value)) (predf p value)
                                        Nothing    -> do 
                                                        tell [Got (mv, (show mv))]
                                                        return $ FailingPredicate "Type error!"
                            (Sent x, Send p cont) -> 
                                do
                                    let Just x' = extract x
                                    value <- lift $ lift $ shrinkValue p x'
                                    lift $ lift $ put ch $ (embed value)
                                    tell [Sent (embed value, (show value))]
                                    c <- lift $ cont value
                                    sessionShrink (n-1) xs c ch
    | otherwise         = sessionShrink n xs st ch 
-- We have given up on following the trace
sessionShrink n [] (Send p cont) ch =
    do
        value <- lift $ lift $ generate (generator p)
        lift $ lift $ put ch $ (embed value)
        tell [Sent (embed value, (show value))]
        c <- lift $ cont value
        sessionShrink (n-1) [] c ch
sessionShrink n [] (Get p cont) ch =
    do
        mv <- lift $ lift $ BCH.get ch
        case extract mv of
            Just value -> if predf p value == Nothing then
                            do
                                tell [Got (mv, (show value))]
                                c <- lift $ cont value
                                sessionShrink (n-1) [] c ch
                          else
                            do
                                tell [Got (mv, (show value))]
                                return $ fromMaybeSS $ fmap (++" "++(show value)) (predf p value)
            Nothing    -> do 
                            tell [Got (mv, (show mv))]
                            return $ FailingPredicate "Type error!"

shrinkValue :: (Arbitrary a) => Predicate a -> a -> IO a
shrinkValue p a =
  let shr = maybe ((filter ((== Nothing) . (predf p))) . shrink) id (shrinker p) in
  case take 50 $ shr a of
    [] -> return a
    xs -> generate $ oneof (map return xs)

traceMatch :: Interaction t -> ST m t -> Bool
traceMatch (Got x) (Get _ _) = True
traceMatch (Sent x) (Send p _) =
    let x' = extract x in
    case x' of
        Just a  -> predf p a == Nothing -- Predicate holds / doesn't hold
        Nothing -> False -- Type error
traceMatch _ _ = False

runErlangT :: (Erlang t, Show t, MonadTrans m, Monad (m IO))
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> CSpecT m t a -- The session type for the interaction
          -> (forall a. m IO a -> IO a)
          -> IO ()
runErlangT self mod fun spec interp =
    do
        st <- interp $ runContT spec (fmap return $ const End)
        specCheck (runfun self) st interp
    where
        runfun :: Erlang t => Self -> P Chan t -> IO ()
        runfun self ch =
            do
                mbox <- createMBox self
                let pid = mboxSelf mbox
                rpcCall mbox (Short "erl") mod fun []
                mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox)
                id1 <- forkIO $ erlangLoop ch mbox
                id2 <- forkIO $ haskellLoop ch mbox
                waitToBeKilled ch
                finish mbox id1 id2
                killAcc ch

        finish mbox id1 id2 =
            do
                killThread id1
                killThread id2
                pid <- rpcCall mbox (Short "erl") "erlang" "whereis" [ErlAtom "p"]
                rpcCall mbox (Short "erl") "erlang" "exit" [pid, ErlAtom "ok"]

        erlangLoop ch mbox =
            do
               m <- mboxRecv mbox
               put ch $ fromErlang m
               erlangLoop ch mbox
        
        haskellLoop ch mbox =
            do
                m <- BCH.get ch
                mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox, m)
                haskellLoop ch mbox

runErlang :: (Erlang t, Show t)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> CSpec t a -- The session type for the interaction
          -> IO ()
runErlang self mod fun spec = runErlangS self mod fun spec ()

runErlangS :: (Erlang t, Show t)
           => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> CSpecS st t a -- The session type for the interaction
          -> st
          -> IO ()
runErlangS self mod fun spec state = runErlangT self mod fun spec (flip S.evalStateT state)

runShellTCPT :: (MonadTrans m, Monad (m IO))
             => String -- shell command
             -> CSpecT m String a -- The session type for the interaction
             -> (forall a. m IO a -> IO a) -- Interpretation function for the monad transformer
             -> IO ()
runShellTCPT prog spec interp =
    do
        st <- interp $ runContT spec (fmap return $ const End)
        specCheck runfun st interp
    where
        startup = do
          ph <- spawnCommand prog
          threadDelay 2000
          ec <- getProcessExitCode ph
          case ec of
            Nothing -> return ph
            _       -> startup

        connect ph ch =
          N.connect "localhost" "6060" $ \(socket, _) ->
              do
                id1 <- forkIO $ programLoop socket ch
                id2 <- forkIO $ haskellLoop socket ch
                waitToBeKilled ch
                finish id1 id2
                terminateProcess ph
                waitForProcess ph
                killAcc ch

        runfun :: P Chan String -> IO ()
        runfun ch = do
          ph <- startup
          connect ph ch 

        finish id1 id2 =
            do
                killThread id1
                killThread id2

        programLoop socket ch =
            do
               msg <- readLineFrom socket
               put ch $ msg
               programLoop socket ch
        
        haskellLoop socket ch =
            do
                m <- BCH.get ch
                N.send socket (BS.pack $ embed m ++ "\n")
                haskellLoop socket ch

runShellTCPS :: String -> CSpecS st String a -> st -> IO ()
runShellTCPS prog spec st = runShellTCPT prog spec (flip S.evalStateT st)

runShellTCP :: String -> CSpec String a -> IO ()
runShellTCP prog spec = runShellTCPS prog spec ()

readLineFrom :: N.Socket -> IO String
readLineFrom = doTheReading ""
  where
    doTheReading xs socket = do
      c <- fmap (fmap (head . BS.unpack)) $ N.recv socket 1
      case c of
        Just '\n' -> return (reverse xs)
        Just x    -> doTheReading (x:xs) socket
        Nothing   -> return (reverse xs)

-- Run some tests
specCheck :: (BiChannel ch c, Show c, MonadTrans m, Monad (m IO))
          => (ch c -> IO ()) -- Function to test
          -> ST m c                     -- The session type for the interaction
          -> (forall a. m IO a -> IO a) 
          -> IO ()
specCheck impl t interp = loop m
    where
        -- Number of tests to pass
        m = 100
        -- Number of shrinking passes
        k = 200 
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    (b, w) <- interp $ runWriterT $ sessionTest t ch
                    kill ch
                    if b == Nothing then
                        loop (n-1)
                    else
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "Failed after "++(show (m - n))++" tests"
                            shrinkLoop 0 (fromJust b) w
                            putStrLn $ "\n~~~~~\n"
                            shrinkLoop k (fromJust b) w

        shrinkLoop 0 s trace =
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "With: "++s
                            putStrLn "In:"
                            putStrLn "---"
                            sequence_ $ map (putStrLn . ("    "++) . printTrace) (map (fmap snd) trace)
                            putStrLn "---"
                            return ()
        shrinkLoop n s trace =
                        do
                            hPutStr stderr $ "\rShrinking ("++(show n)++")...               \r"
                            ch <- new
                            forkIO $ impl (bidirect ch)
                            (b, w) <- fmap interp runWriterT $ sessionShrink (length trace) (map (fmap fst) trace) t ch
                            kill ch
                            case b of
                                FailedToShrink -> shrinkLoop (n-1) s trace
                                FailingPredicate s' -> shrinkLoop (n-1) s' w

checkCoherence :: (MonadTrans m, Monad (m IO)) => ST m c -> WriterT (Log (c, String)) (m IO) Bool
checkCoherence (Send p cont) =
    do
        mv <- lift $ lift $ tryGen (generator p)
        case mv of
            Nothing    -> lift $ lift $ do
                                         hPutStr stderr $ "\r                  \r"
                                         putStrLn $ "Failed with inability to generate: " ++ name p
                                         return False
            Just value -> case predf p value of
                            Nothing -> do
                                        tell [Sent (embed value, show value)]
                                        c <- lift $ cont value
                                        checkCoherence c
                            Just s  -> lift $ lift $ do
                                                       hPutStr stderr $ "\r                  \r"
                                                       putStrLn "Failed with: "
                                                       putStrLn s
                                                       return False
checkCoherence (Get p cont)  =
    do
        mv <- lift $ lift $ tryGen (generator p)
        case mv of
            Nothing    -> lift $ lift $ do
                                           hPutStr stderr $ "\r                  \r"
                                           putStrLn $ "Failed with inability to generate: " ++ name p
                                           return False
            Just value -> case predf p value of
                            Nothing -> do
                                        tell [Got (embed value, show value)]
                                        c <- lift $ cont value
                                        checkCoherence c
                            Just s  -> lift $ lift $ do
                                                hPutStr stderr $ "\r                  \r"
                                                putStrLn "Failed with: "
                                                putStrLn s
                                                return False
checkCoherence End           = return True

coherentT :: (MonadTrans m, Monad (m IO)) => CSpecT m c a -> (forall a. m IO a -> IO a) -> IO ()
coherentT spec interp = do
                          st <- interp $ runContT spec (fmap return $ const End)
                          coherent' st 100
    where
        coherent' _ 0 = putStrLn "\rPassed"
        coherent' st n = do
                            hPutStr stderr $ "\r                  \r"
                            hPutStr stderr $ show n
                            (b, log) <- interp $ runWriterT $ checkCoherence st
                            if b then
                                coherent' st (n-1)
                            else
                                do
                                    putStrLn "In:"
                                    putStrLn "---"
                                    sequence_ $ map (putStrLn . ("    "++) . printTrace) (map (fmap snd) log)
                                    return ()

coherentS :: CSpecS st c a -> st -> IO () 
coherentS spec state = coherentT spec (flip S.evalStateT state)

coherent :: CSpec c a -> IO ()
coherent csp = coherentS csp ()

-- Try to generate a value, if it is not done in 1 second, give up
tryGen :: (NFData a) => Gen a -> IO (Maybe a)
tryGen gen = catch (timeout 1000000 (specialGenerate gen)) (\(x :: SomeException) -> (return Nothing))

-- We need to do this because `generate gen` generates a thunk
-- which starts getting evaluated, this messes up `timeout`,
-- so we instead defer generation of the thunk.
specialGenerate :: (NFData a) => Gen a -> IO a
specialGenerate gen =
    do
        v <- generate gen
        v `deepseq` return v

printTrace (Got x)   = "Got:  "++x
printTrace (Sent x)  = "Sent: "++x
