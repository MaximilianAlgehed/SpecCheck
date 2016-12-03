module Predicate where
import Test.QuickCheck
import Control.Monad
import Data.List

data Predicate a = Predicate {generator :: Gen a, predf :: a -> Maybe String, name :: String}

predicate :: String -> (Gen a, (a -> Bool)) -> Predicate a
predicate s (g, p) = Predicate g (\a -> guard (not (p a)) >> return s) s

-- Some generator-predicate pairs
posNum :: (Ord a, Num a, Arbitrary a) => Predicate a
posNum = predicate "posNum " ((fmap abs arbitrary) `suchThat` (>0), (>0))

negNum :: (Ord a, Num a, Arbitrary a) => Predicate a
negNum = predicate "negNum " ((fmap (negate . abs) arbitrary) `suchThat` (<0), (<0))

lessThan :: (Ord a, Arbitrary a, Show a) => a -> Predicate a
lessThan v = predicate (show v ++ " > ") (arbitrary `suchThat` (<v), (<v))

is :: (Eq a, Show a) => a -> Predicate a
is a = predicate ("is "++(show a)) (return a,(a==))

permutationOf :: (Ord a, Show a) => [a] -> Predicate [a]
permutationOf bs = predicate ("permutationOf " ++ (show bs)) (shuffle bs, (((sort bs) ==) . sort))

inRange :: (Ord a, Show a, Arbitrary a) => (a, a) -> Predicate a
inRange (l, h) = predicate ("inRange "++(show (l, h))) (arbitrary `suchThat` (\x -> x >= l && x <= h), (\x -> x >= l && x <= h))

(|||) :: Predicate a -> Predicate a -> Predicate a
(Predicate lg l ln) ||| (Predicate rg r rn) = Predicate (oneof [lg, rg]) (disj l r) (ln ++ "|||" ++ rn)
    where
        disj l r a = do
                        sl <- l a
                        sr <- r a
                        return $ "("++ sl ++ " || " ++ sr ++ ")"

(&&&) :: Predicate a -> Predicate a -> Predicate a
(Predicate lg l ln) &&& (Predicate rg r rn) = Predicate (oneof [lg, rg] `suchThat` (\a -> p a == Nothing)) p (ln ++ "&&&" ++ rn)
    where
        p a = case r a of
                Nothing -> case l a of
                            Nothing -> Nothing
                            Just s  -> Just s
                Just s  -> Just s

(.*.) :: Predicate a -> Predicate b -> Predicate (a, b)
(Predicate lg l ln) .*. (Predicate rg r rn) = Predicate (do
                                                            l <- lg
                                                            r <- rg
                                                            return (l, r))
                                                        p
                                                        (ln ++ " .*. " ++ rn)
    where
        p (a, b) = case r b of
                        Nothing -> case l a of
                                    Nothing -> Nothing
                                    Just s  -> Just s
                        Just s  -> Just s

wildcard :: (Arbitrary a) => Predicate a
wildcard = Predicate arbitrary (const Nothing) "_"
