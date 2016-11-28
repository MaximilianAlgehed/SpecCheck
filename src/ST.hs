{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}
module ST where
import System.Timeout
import Predicate
import Debug.Trace
import Prelude hiding (any)
import Test.QuickCheck
import Data.List hiding (any)
import Foreign.Erlang
import BiCh 
import System.IO
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan
import Typeclasses

erlBool True = ErlAtom "true"
erlBool False = ErlAtom "false"

-- It would be good to get a monad in here so we can
-- do some side-effects, like reading the database to
-- check if what the server is saying is actually true
-- etc.
data ST c where
    Send   :: (Arbitrary a, Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c
    Get    :: (Arbitrary a, Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c 
    Choose :: Gen Int -> [(String, ST c)] -> ST c
    Branch :: Gen Int -> [(String, ST c)] -> ST c
    End    :: ST c

dual :: ST a -> ST a
dual (Send pred cont) = Get pred (dual . cont)
dual (Get pred cont)  = Send pred (dual . cont)
dual (Choose gen cs)  = Branch gen (map (\(s,t) -> (s, dual t)) cs)
dual (Branch gen cs)  = Choose gen (map (\(s,t) -> (s, dual t)) cs)
dual End              = End

sessionTest :: (Show c, BiChannel ch c)
            => ST c
            -> ch (Protocol c)
            -> WriterT (Log (c, String)) IO (Maybe String)
sessionTest (Send p cont) ch =
    do
        value <- lift $ generate (generator p)
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value, show value))]
        sessionTest (cont value) ch
sessionTest (Get p cont) ch =
    do
        Pure mv <- lift $ get ch
        case extract mv of
            Just value -> if predf p value == Nothing then
                            do
                                tell [Got (Pure (mv, show value))]
                                sessionTest (cont value) ch
                          else
                            do
                                tell [Got (Pure (mv, show value))]
                                return $ fmap (++" "++(show value)) (predf p value)
            Nothing    -> do 
                            tell [Got (Pure (mv, show mv))]
                            return $ Just "Type error!"
sessionTest (Choose gen choices) ch =
    do
        choice <- lift $ generate gen
        let (name, cont) = choices!!(choice `mod` (length choices))
        tell [Sent (Choice name)]
        lift $ put ch (Choice name)
        sessionTest cont ch
sessionTest (Branch _ choices) ch =
    do
        choice <- lift $ get ch
        case choice of
            Choice s
                | s `elem` (map fst choices) -> do
                                                    tell [Got (Choice s)]
                                                    sessionTest (fromJust (lookup s choices)) ch
                | otherwise -> do
                                tell [Got (Choice s)]
                                return $ Just "Tried to make invalid call!"
            Pure v -> do
                    tell [Got $ Pure (v, show v)]
                    return $ Just "Type error!"
sessionTest End _ = return Nothing

data ShrinkStatus = FailedToShrink | FailingPredicate String

fromMaybeSS :: Maybe String -> ShrinkStatus
fromMaybeSS Nothing = FailedToShrink
fromMaybeSS (Just s) = FailingPredicate s

sessionShrink :: (Show c, BiChannel ch c)
            => Int
            -> Log c
            -> ST c
            -> ch (Protocol c)
            -> WriterT (Log (c, String)) IO ShrinkStatus -- Fix the return type to be "Log (c, String)" instead
                                                         -- of "Log String"
sessionShrink 0 _ _   _ = return FailedToShrink -- Our trace is longer than the original trace
sessionShrink _ _ End _ = return FailedToShrink -- We didn't failsify the property
sessionShrink n (x:xs) st ch
    | traceMatch x st   = case (x, st) of
                            (Got (Pure _), Get (_, pred) cont) ->
                                do
                                    Pure mv <- lift $ get ch
                                    case extract mv of
                                        Just value -> if pred value == Nothing then
                                                        do
                                                            tell [Got (Pure (mv, (show value)))]
                                                            sessionShrink (n-1) xs (cont value) ch
                                                      else
                                                        do
                                                            tell [Got (Pure (mv, (show value)))]
                                                            return $ fromMaybeSS $ fmap (++" "++(show value)) (pred value)
                                        Nothing    -> do 
                                                        tell [Got (Pure (mv, (show mv)))]
                                                        return $ FailingPredicate "Type error!"
                            (Got (Choice s), Branch _ choices) ->
                                do
                                    choice <- lift $ get ch
                                    case choice of
                                        Choice s
                                            | s `elem` (map fst choices) ->
                                                do
                                                  tell [Got (Choice s)]
                                                  sessionShrink (n-1) xs (fromJust (lookup s choices)) ch
                                            | otherwise ->
                                                do
                                                  tell [Got (Choice s)]
                                                  return $ FailingPredicate "Tried to make invalid call!"
                                        Pure v -> do
                                                tell [Got (Pure (v, (show v)))]
                                                return $ FailingPredicate "Type error!"
                            (Sent (Pure x), Send (gen, pred) cont) -> 
                                do
                                    let Just x' = extract x
                                    value <- lift $ shrinkValue gen pred x'
                                    lift $ put ch $ Pure (embed value)
                                    tell [Sent (Pure (embed value, (show value)))]
                                    sessionShrink (n-1) xs (cont value) ch
                            (_, Choose gen choices) ->
                                do
                                    choice <- lift $ generate gen
                                    let (name, cont) = choices!!(choice `mod` (length choices))
                                    tell [Sent (Choice name)]
                                    lift $ put ch (Choice name)
                                    sessionShrink (n-1) xs cont ch
    | otherwise         = sessionShrink n xs st ch 
-- We have given up on following the trace
sessionShrink n [] (Send (gen, _) cont) ch =
    do
        value <- lift $ generate gen
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (embed value, (show value)))]
        sessionShrink (n-1) [] (cont value) ch
sessionShrink n [] (Get (_, pred) cont) ch =
    do
        Pure mv <- lift $ get ch
        case extract mv of
            Just value -> if pred value == Nothing then
                            do
                                tell [Got (Pure (mv, (show value)))]
                                sessionShrink (n-1) [] (cont value) ch
                          else
                            do
                                tell [Got (Pure (mv, (show value)))]
                                return $ fromMaybeSS $ fmap (++" "++(show value)) (pred value)
            Nothing    -> do 
                            tell [Got (Pure (mv, (show mv)))]
                            return $ FailingPredicate "Type error!"
sessionShrink n [] (Choose gen choices) ch =
    do
        choice <- lift $ generate gen
        let (name, cont) = choices!!(choice `mod` (length choices))
        tell [Sent (Choice name)]
        lift $ put ch (Choice name)
        sessionShrink (n-1) [] cont ch
sessionShrink n [] (Branch _ choices) ch =
    do
        choice <- lift $ get ch
        case choice of
            Choice s
                | s `elem` (map fst choices) -> do
                                                    tell [Got (Choice s)]
                                                    sessionShrink (n-1) [] (fromJust (lookup s choices)) ch
                | otherwise -> do
                                tell [Got (Choice s)]
                                return $ FailingPredicate "Tried to make invalid call!"
            Pure v -> do
                    tell [Got (Pure (v, (show v)))]
                    return $ FailingPredicate "Type error!"

shrinkValue :: (Arbitrary a) => Gen a -> (a -> Maybe String) -> a -> IO a
shrinkValue gen pred a =
    case [x | x <- take 50 $ shrink a, pred x == Nothing] of
        [] -> generate gen
        xs -> generate $ oneof (map return xs)

traceMatch :: Interaction (Protocol t) -> ST t -> Bool
traceMatch (Got (Pure x)) (Get _ _) = True
traceMatch (Sent (Pure x)) (Send (gen, p) _) =
    let x' = extract x in
    case x' of
        Just a  -> p a == Nothing -- Predicate holds / doesn't hold
        Nothing -> False -- Type error
traceMatch (Got (Choice s)) (Branch _ xs) = s `elem` [fst x | x <- xs]
traceMatch (Sent (Choice s)) (Choose _ xs) = s `elem` [fst x | x <- xs]
traceMatch _ _ = False

runErlang :: (t :<: ErlType, Show t)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> ST t -- The session type for the interaction
          -> IO ()
runErlang self mod fun st = specCheck (runfun self) st
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
                m <- get ch
                mboxSend mbox (Short "erl") (Right "p") (mboxSelf mbox, m)
                haskellLoop ch mbox

-- Run some tests
specCheck :: (BiChannel ch c, Show c)
          => (ch (Protocol c) -> IO ()) -- Function to test
          -> ST c                       -- The session type for the interaction
          -> IO ()
specCheck impl t = loop 100
    where
        loop 0 = putStrLn "\rO.K"
        loop n = do
                    hPutStr stderr $ "\r                  \r"
                    hPutStr stderr $ show n
                    ch <- new
                    forkIO $ impl (bidirect ch)
                    (b, w) <- runWriterT $ sessionTest t ch
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
                            (b, w) <- runWriterT $ sessionShrink (length trace) (map (fmap (fmap fst)) trace) t ch
                            kill ch
                            case b of
                                FailedToShrink -> shrinkLoop (n-1) s trace
                                FailingPredicate s' -> shrinkLoop (n-1) s' w

checkCoherence :: ST c -> IO Bool
checkCoherence (Send p cont) =
    do
        mv <- tryGen (generator p)
        case mv of
            Nothing    -> do
                            putStrLn $ "Failed with inability to generate: " ++ name p
                            return False
            Just value -> case predf p value of
                            Nothing -> checkCoherence (cont value)
                            Just s  -> do
                                        putStrLn "Failed with: "
                                        putStrLn s
                                        return False
checkCoherence (Get p cont)     = checkCoherence (Send p cont)
checkCoherence (Branch g conts) =
    do
        mi <- tryGen g
        case mi of
            Nothing -> return False
            Just i  -> checkCoherence $ snd (conts !! i)
checkCoherence (Choose g conts) = checkCoherence (Branch g conts)
checkCoherence End              = return True

tryGen :: Gen a -> IO (Maybe a)
tryGen = (timeout 1000000) . generate 

maxLen = maximum . (map (length . extract))
    where
        extract (Got (Pure s)) = s
        extract (Got (Choice s)) = s
        extract (Sent (Pure s)) = s
        extract (Sent (Choice s)) = s

prettyTrace n (Got (Pure x))    = ["|" ++ middle n x ++ "  |", "|<"++ line '-' n  ++ "|", "|"++line ' ' n ++ " |"]
prettyTrace n (Got (Choice s))  = ["|" ++ middle n s ++ "  |", "|<"++ line '.' n ++ "  |", "|"++line ' ' n ++ " |"]
prettyTrace n (Sent (Pure x))   = ["|" ++ middle n x ++ "  |", "|"++ line '-' n  ++ ">|", "|"++line ' ' n ++ " |"]
prettyTrace n (Sent (Choice s)) = ["|" ++ middle n s ++ "  |", "|"++ line '.' n ++ ">|", "|"++line ' ' n ++ " |"]

middle n s = (line ' ' m) ++ s ++ (line ' ' k)
    where
        m = if even (n - length s) then
                (n - length s) `div` 2
            else
                (n - length s) `div` 2 + 1
        k = (n - length s) `div` 2

line c n = concat $ replicate (n - 1) $ [c]

printTrace (Got (Pure x))   = "Got ("++x++")"
printTrace (Got (Choice s)) = "Branched "++s
printTrace (Sent (Pure x))  = "Sent ("++x++")"
printTrace (Sent (Choice s)) = "Chose "++s

-- Some syntax
l <|> r = Choose (oneof [return 0, return 1]) [l, r]
infixr 1 <|>

l <&> r = Branch (oneof [return 0, return 1]) [l, r]
infixr 1 <&>

f .- c = f (const c)
infixr 0 .-
