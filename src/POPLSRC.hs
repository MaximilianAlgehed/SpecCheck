{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}
import ST
import Typeclasses
import Foreign.Erlang
import Test.QuickCheck

{- The new example from POPL SRC -}
protocol :: ST ErlType 
protocol = Send validData .-
           ("execute", execActs) <&> ("continue", continue)

execActs :: ST ErlType 
execActs =
    Get validActs $ \acts ->
    Send (validStatus acts) .-
    continue

continue :: ST ErlType 
continue = ("stayAwake", protocol) <|> ("stop", End)

validData = predicate "validData" (sequence $ [arbitrary, arbitrary, arbitrary], \xs -> length (xs :: [Double]) == 3 )

data Action = Output Int Int | Input Int

instance Action :<: ErlType where
    embed (Output i j) = ErlTuple [ErlAtom "output", ErlInt i, ErlInt j]
    embed (Input i)    = ErlTuple [ErlAtom "input", ErlInt i]

    extract (ErlTuple [ErlAtom "output", ErlInt i, ErlInt j]) = Just $ Output i j
    extract (ErlTuple [ErlAtom "input", ErlInt i]) = Just $ Input i
    extract _ = Nothing

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

instance Arbitrary Status where
    arbitrary = oneof [Out <$> arbitrary <*> arbitrary, Inp <$> arbitrary <*> arbitrary]

instance Status :<: ErlType where
    embed (Out i b) = ErlTuple [ErlAtom "out", ErlInt i, erlBool b]
    embed (Inp i d) = ErlTuple [ErlAtom "inp", ErlInt i, ErlFloat d]

    extract (ErlTuple [ErlAtom "out", ErlInt i, ErlAtom tf])
        | tf == "true" = Just $ Out i True
        | tf == "false" = Just $ Out i False
    extract (ErlTuple [ErlAtom "inp", ErlInt i, ErlFloat d]) = Just $ Inp i d
    extract _ = Nothing

instance Show Status where
    show (Out i b) = "Output "++(show i)++" "++(if b then "OK" else "ERR")
    show (Inp i d) = "Input "++(show i)++" "++(show d)

validActs = wildcard

validStatus acts = predicate ("validStatus " ++ (show acts))
    (sequence (map mkGen acts), \xs -> and $ zipWith isValid acts xs)
        where
            isValid (Output _ _) (Inp _ _) = False
            isValid (Output i _) (Out j _) = i == j
            isValid (Input i) (Out _ _) = False
            isValid (Input i) (Inp j _)    = i == j
            
            mkGen (Output i _) = fmap (Out i) arbitrary
            mkGen (Input i)    = fmap (Inp i) arbitrary
