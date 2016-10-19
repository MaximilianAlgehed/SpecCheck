{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}
module ST where
import Prelude hiding (any)
import Test.QuickCheck
import Data.List hiding (any)
import Foreign.Erlang
import BiCh 
import System.IO
import Control.Concurrent
import Control.Monad.Writer.Lazy
import Control.Concurrent.Chan

data Choice = L | R

instance Arbitrary Choice where
    arbitrary = do
                x <- arbitrary
                if x then
                    return L
                else
                    return R

class a :<: b where
    embed   :: a -> b
    extract :: b -> Maybe a

instance a :<: a where
    embed   = id
    extract = Just . id

type Predicate a = (Gen a, a -> Maybe String)

predicate :: String -> (Gen a, (a -> Bool)) -> Predicate a
predicate s (g, p) = (g, \a -> guard (not (p a)) >> return s)

instance (Erlang a) => a :<: ErlType where
    embed = toErlang
    extract = Just . fromErlang

erlBool True = ErlAtom "true"
erlBool False = ErlAtom "false"

data ST c where
    Send   :: (Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c
    Get    :: (Show a, a :<: c) => Predicate a -> (a -> ST c) -> ST c 
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
            -> WriterT (Log String) IO (Maybe String)
sessionTest (Send (gen, _) cont) ch =
    do
        value <- lift $ generate gen
        lift $ put ch $ Pure (embed value)
        tell [Sent (Pure (show value))]
        sessionTest (cont value) ch
sessionTest (Get (_, pred) cont) ch =
    do
        Pure mv <- lift $ get ch
        case extract mv of
            Just value -> if pred value == Nothing then
                            do
                                tell [Got (Pure (show value))]
                                sessionTest (cont value) ch
                          else
                            do
                                tell [Got (Pure (show value))]
                                return $ fmap (++" "++(show value)) (pred value)
            Nothing    -> do 
                            tell [Got (Pure (show mv))]
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
            _ -> do
                    tell [Got (Pure (show choice))]
                    return $ Just "Type error!"
sessionTest End _ = return Nothing

-- | So that we can talk to Erlang!
instance (Erlang t) => Erlang (Protocol t) where
    toErlang (Pure t)    = ErlTuple [ErlAtom "pure", toErlang t]
    toErlang (Choice s)  = ErlTuple [ErlAtom "choice", ErlAtom s]

    fromErlang (ErlTuple [ErlAtom "pure", t])           = Pure (fromErlang t)
    fromErlang (ErlTuple [ErlAtom "choice", ErlAtom s]) = Choice s

runErlang :: (Erlang t, Show t)
          => Self -- Created by "createSelf \"name@localhost\""
          -> String -- module name
          -> String -- function name
          -> ST t -- The session type for the interaction
          -> IO ()
runErlang self mod fun st = specCheck (runfun self) st
    where
        runfun :: (Erlang r) => Self -> P Chan (Protocol r) -> IO ()
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
                return ()

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
                            putStrLn $ "With: "++(fromJust b)
                            putStrLn "In:"
                            putStrLn "---"
                            sequence_ $ map (putStrLn . ("    "++) . printTrace) w
                            --sequence_ $ map (putStrLn . ("    "++)) $ concat $ map (prettyTrace (2*(maxLen w))) w
                            putStrLn "---"
                            return ()
                            where

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

fromJust (Just x) = x

-- Some generator-predicate pairs
posNum :: (Ord a, Num a, Arbitrary a) => Predicate a
posNum = predicate "posNum" (fmap ((+1) . abs) arbitrary, (>0))

is :: (Eq a, Show a) => a -> Predicate a
is a = predicate ("is "++(show a)) (return a,(a==))

isPermutation :: (Ord a, Show a) => [a] -> Predicate [a]
isPermutation bs = predicate ("isPermutation " ++ (show bs)) (shuffle bs, (((sort bs) ==) . sort))

inRange :: (Ord a, Show a, Arbitrary a) => (a, a) -> Predicate a
inRange (l, h) = predicate ("inRange "++(show (l, h))) (arbitrary `suchThat` (\x -> x >= l && x <= h), (\x -> x >= l && x <= h))

(|||) :: Predicate a -> Predicate a -> Predicate a
(lg, l) ||| (rg, r) = (oneof [lg, rg], disj l r)
    where
        disj l r a = do
                        sl <- l a
                        sr <- r a
                        return $ "("++ sl ++ " || " ++ sr ++ ")"

(&&&) :: Predicate a -> Predicate a -> Predicate a
(lg, l) &&& (rg, r) = (oneof [lg, rg] `suchThat` (\a -> p a == Nothing), p)
    where
        p a = case r a of
                Nothing -> case l a of
                            Nothing -> Nothing
                            Just s  -> Just s
                Just s  -> Just s

any :: (Arbitrary a) => Predicate a
any = (arbitrary, const Nothing)

l <|> r = Choose (oneof [return 0, return 1]) [l, r]
infixr 1 <|>

l <&> r = Branch (oneof [return 0, return 1]) [l, r]
infixr 1 <&>

f .- c = f (const c)
infixr 0 .-

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

bookShop' bs = Send any $
               \b -> let bs' = b:bs in
               ("another", bookShop' bs')
               <|>
               ("request", Get (isPermutation bs') cont)

cont bs = ("another", bookShop' bs) <|> ("done", End)

{- The new example from POPL SRC -}
protocol :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
protocol = Send validData .-
           ("execute", execActs) <&> ("continue", continue)

execActs :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
execActs =
    Get validActs $ \acts ->
    Send (validStatus acts) .-
    continue

continue :: ([Action] :<: t, [Status] :<: t, [Double] :<: t) => ST t
continue = ("stayAwake", protocol) <|> ("stop", End)

validData = predicate "validData" (sequence $ [arbitrary, arbitrary, arbitrary], \xs -> length (xs :: [Double]) == 3 )

data Action = Output Int Int | Input Int

instance Erlang Action where
    toErlang (Output i j) = ErlTuple [ErlAtom "output", ErlInt i, ErlInt j]
    toErlang (Input i)    = ErlTuple [ErlAtom "input", ErlInt i]

    fromErlang (ErlTuple [ErlAtom "output", ErlInt i, ErlInt j]) = Output i j
    fromErlang (ErlTuple [ErlAtom "input", ErlInt i]) = Input i

instance Show Action where
    show (Output i j) = "Output "++(show i)++" high for "++(show j)++" seconds"
    show (Input i)    = "Get input "++(show i)

instance Arbitrary Action where
    arbitrary = oneof [do
                        x <- arbitrary `suchThat` (\x -> x >= 0 && x <= 10)
                        y <- arbitrary `suchThat` (\x -> x >= 0)
                        return $ Output x y,
                       fmap Input (arbitrary `suchThat` (\x -> x >= 0 && x <= 10))]

data Status = Out Int Bool | Inp Int Double

instance Erlang Status where
    toErlang (Out i b) = ErlTuple [ErlAtom "out", ErlInt i, erlBool b]
    toErlang (Inp i d) = ErlTuple [ErlAtom "inp", ErlInt i, ErlFloat d]

    fromErlang (ErlTuple [ErlAtom "out", ErlInt i, ErlAtom tf])
        | tf == "true" = Out i True
        | tf == "false" = Out i False
    fromErlang (ErlTuple [ErlAtom "inp", ErlInt i, ErlFloat d]) = Inp i d

instance Show Status where
    show (Out i b) = "Output "++(show i)++" "++(if b then "OK" else "ERR")
    show (Inp i d) = "Input "++(show i)++" "++(show d)

validActs = any

validStatus acts = predicate ("validStatus " ++ (show acts))
    (sequence (map mkGen acts), \xs -> and $ zipWith isValid acts xs)
        where
            isValid (Output _ _) (Inp _ _) = False
            isValid (Output i _) (Out j _) = i == j
            isValid (Input i) (Out _ _) = False
            isValid (Input i) (Inp j _)    = i == j
            
            mkGen (Output i _) = fmap (Out i) arbitrary
            mkGen (Input i)    = fmap (Inp i) arbitrary
