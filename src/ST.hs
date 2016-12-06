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
             UndecidableInstances #-}
module ST where
import CSpec
import Control.Monad.Trans
import Control.DeepSeq
import Predicate
import System.Timeout
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

erlBool True = ErlAtom "true"
erlBool False = ErlAtom "false"

dual :: (MonadTrans m, Functor (m IO)) => ST m a -> ST m a
dual (Send pred cont) = Get pred $ \a -> fmap dual (cont a)
dual (Get pred cont)  = Send pred $ \a -> fmap dual (cont a)
dual End              = End

sessionTest :: (Show c, BiChannel ch c, MonadTrans m, Monad (m IO))
            => ST m c
            -> ch (Protocol c)
            -> WriterT (Log (c, String)) (m IO) (Maybe String)
sessionTest (Send p cont) ch =
    do
        value <- lift $ lift $ generate (generator p)
        lift $ lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value, show value))]
        c <- lift $ cont value
        sessionTest c ch
sessionTest (Get p cont) ch =
    do
        Pure mv <- lift $ lift $ BCH.get ch
        case extract mv of
            Just value -> if predf p value == Nothing then
                            do
                                tell [Got (Pure (mv, show value))]
                                c <- lift $ cont value
                                sessionTest c ch
                          else
                            do
                                tell [Got (Pure (mv, show value))]
                                return $ fmap (++" "++(show value)) (predf p value)
            Nothing    -> do 
                            tell [Got (Pure (mv, show mv))]
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
            -> ch (Protocol c)
            -> WriterT (Log (c, String)) (m IO) ShrinkStatus -- Fix the return type to be "Log (c, String)" instead
                                                         -- of "Log String"
sessionShrink 0 _ _   _ = return FailedToShrink -- Our trace is longer than the original trace
sessionShrink _ _ End _ = return FailedToShrink -- We didn't failsify the property
sessionShrink n (x:xs) st ch
    | traceMatch x st   = case (x, st) of
                            (Got (Pure _), Get p cont) ->
                                do
                                    Pure mv <- lift $ lift $ BCH.get ch
                                    case extract mv of
                                        Just value -> if predf p value == Nothing then
                                                        do
                                                            tell [Got (Pure (mv, (show value)))]
                                                            c <- lift $ cont value
                                                            sessionShrink (n-1) xs c ch
                                                      else
                                                        do
                                                            tell [Got (Pure (mv, (show value)))]
                                                            return $ fromMaybeSS $ fmap (++" "++(show value)) (predf p value)
                                        Nothing    -> do 
                                                        tell [Got (Pure (mv, (show mv)))]
                                                        return $ FailingPredicate "Type error!"
                            (Sent (Pure x), Send p cont) -> 
                                do
                                    let Just x' = extract x
                                    value <- lift $ lift $ shrinkValue (generator p) (predf p) x'
                                    lift $ lift $ put ch $ Pure (embed value)
                                    tell [Sent (Pure (embed value, (show value)))]
                                    c <- lift $ cont value
                                    sessionShrink (n-1) xs c ch
    | otherwise         = sessionShrink n xs st ch 
-- We have given up on following the trace
sessionShrink n [] (Send p cont) ch =
    do
        value <- lift $ lift $ generate (generator p)
        lift $ lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value, (show value)))]
        c <- lift $ cont value
        sessionShrink (n-1) [] c ch
sessionShrink n [] (Get p cont) ch =
    do
        Pure mv <- lift $ lift $ BCH.get ch
        case extract mv of
            Just value -> if predf p value == Nothing then
                            do
                                tell [Got (Pure (mv, (show value)))]
                                c <- lift $ cont value
                                sessionShrink (n-1) [] c ch
                          else
                            do
                                tell [Got (Pure (mv, (show value)))]
                                return $ fromMaybeSS $ fmap (++" "++(show value)) (predf p value)
            Nothing    -> do 
                            tell [Got (Pure (mv, (show mv)))]
                            return $ FailingPredicate "Type error!"

shrinkValue :: (Arbitrary a) => Gen a -> (a -> Maybe String) -> a -> IO a
shrinkValue gen pred a =
    case [x | x <- take 50 $ shrink a, pred x == Nothing] of
        [] -> generate gen
        xs -> generate $ oneof (map return xs)

traceMatch :: Interaction (Protocol t) -> ST m t -> Bool
traceMatch (Got (Pure x)) (Get _ _) = True
traceMatch (Sent (Pure x)) (Send p _) =
    let x' = extract x in
    case x' of
        Just a  -> predf p a == Nothing -- Predicate holds / doesn't hold
        Nothing -> False -- Type error
traceMatch _ _ = False

runErlangT :: (t :<: ErlType, Show t, MonadTrans m, Monad (m IO))
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
        runfun :: (t :<: ErlType) => Self -> P Chan (Protocol t) -> IO ()
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

runErlang :: (t :<: ErlType, Show t)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> CSpec t a -- The session type for the interaction
          -> IO ()
runErlang self mod fun spec = runErlangT self mod fun spec runIdentityT

-- Run some tests
specCheck :: (BiChannel ch c, Show c, MonadTrans m, Monad (m IO))
          => (ch (Protocol c) -> IO ()) -- Function to test
          -> ST m c                     -- The session type for the interaction
          -> (forall a. m IO a -> IO a) 
          -> IO ()
specCheck impl t interp = loop 100
    where
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    (b, w) <- fmap interp runWriterT $ sessionTest t ch
                    kill ch
                    if b == Nothing then
                        loop (n-1)
                    else
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "Failed after "++(show (100 - n))++" tests"
                            shrinkLoop 0 (fromJust b) w
                            putStrLn $ "\n~~~~~\n"
                            shrinkLoop 100 (fromJust b) w

        shrinkLoop 0 s trace =
                        do
                            hPutStr stderr $ "\r                  \r"
                            putStrLn $ "With: "++s
                            putStrLn "In:"
                            putStrLn "---"
                            sequence_ $ map (putStrLn . ("    "++) . printTrace) (map (fmap (fmap snd)) trace)
                            --sequence_ $ map (putStrLn . ("    "++)) $ concat $ map (prettyTrace (2*(maxLen w))) w
                            putStrLn "---"
                            return ()
        shrinkLoop n s trace =
                        do
                            hPutStr stderr $ "\rShrinking ("++(show n)++")...               \r"
                            ch <- new
                            forkIO $ impl (bidirect ch)
                            (b, w) <- fmap interp runWriterT $ sessionShrink (length trace) (map (fmap (fmap fst)) trace) t ch
                            kill ch
                            case b of
                                FailedToShrink -> shrinkLoop (n-1) s trace
                                FailingPredicate s' -> shrinkLoop (n-1) s' w

{-
checkCoherence :: ST c -> WriterT (Log (c, String)) IO Bool
checkCoherence (Send p cont) =
    do
        mv <- lift $ tryGen (generator p)
        case mv of
            Nothing    -> lift $ do
                                    hPutStr stderr $ "\r                  \r"
                                    putStrLn $ "Failed with inability to generate: " ++ name p
                                    return False
            Just value -> case predf p value of
                            Nothing -> do
                                        tell [Sent (Pure (embed value, show value))]
                                        checkCoherence (cont value)
                            Just s  -> lift $ do
                                                hPutStr stderr $ "\r                  \r"
                                                putStrLn "Failed with: "
                                                putStrLn s
                                                return False
checkCoherence (Get p cont)  =
    do
        mv <- lift $ tryGen (generator p)
        case mv of
            Nothing    -> lift $ do
                                    hPutStr stderr $ "\r                  \r"
                                    putStrLn $ "Failed with inability to generate: " ++ name p
                                    return False
            Just value -> case predf p value of
                            Nothing -> do
                                        tell [Got (Pure (embed value, show value))]
                                        checkCoherence (cont value)
                            Just s  -> lift $ do
                                                hPutStr stderr $ "\r                  \r"
                                                putStrLn "Failed with: "
                                                putStrLn s
                                                return False
checkCoherence End           = return True

coherent :: ST c -> IO ()
coherent st = coherent' st 100
    where
        coherent' _ 0 = putStrLn "\rPassed"
        coherent' st n = do
                            hPutStr stderr $ "\r                  \r"
                            hPutStr stderr $ show n
                            (b, log) <- runWriterT $ checkCoherence st
                            if b then
                                coherent' st (n-1)
                            else
                                do
                                    putStrLn "In:"
                                    putStrLn "---"
                                    sequence_ $ map (putStrLn . ("    "++) . printTrace) (map (fmap (fmap snd)) log)
                                    return ()

shrinkCoherence :: ST c -> Log (c, String) -> IO ()
shrinkCoherence = undefined

-}

-- Try to generate a value, if it is not done in 1 second, give up
tryGen :: (NFData a) => Gen a -> IO (Maybe a)
tryGen gen = timeout 1000000 (specialGenerate gen)

-- We need to do this because `generate gen` generates a thunk
-- which starts getting evaluated, this messes up `timeout`,
-- so we instead defer generation of the thunk.
specialGenerate :: (NFData a) => Gen a -> IO a
specialGenerate gen =
    do
        v <- generate gen
        v `deepseq` return v

maxLen = maximum . (map (length . extract))
    where
        extract (Got (Pure s)) = s
        extract (Got (Choice s)) = s
        extract (Sent (Pure s)) = s
        extract (Sent (Choice s)) = s

prettyTrace n (Got (Pure x))    = ["|" ++ middle n x ++ "  |", "|<"++ line '-' n  ++ "|", "|"++line ' ' n ++ " |"]
prettyTrace n (Sent (Pure x))   = ["|" ++ middle n x ++ "  |", "|"++ line '-' n  ++ ">|", "|"++line ' ' n ++ " |"]

middle n s = (line ' ' m) ++ s ++ (line ' ' k)
    where
        m = if even (n - length s) then
                (n - length s) `div` 2
            else
                (n - length s) `div` 2 + 1
        k = (n - length s) `div` 2

line c n = concat $ replicate (n - 1) $ [c]

printTrace (Got (Pure x))   = "Got ("++x++")"
printTrace (Sent (Pure x))  = "Sent ("++x++")"
