module Predicate where
import Test.QuickCheck

type Predicate a = (Gen a, a -> Maybe String)

predicate :: String -> (Gen a, (a -> Bool)) -> Predicate a
predicate s (g, p) = (g, \a -> guard (not (p a)) >> return s)

-- Some generator-predicate pairs
posNum :: (Ord a, Num a, Arbitrary a) => Predicate a
posNum = predicate "posNum " ((fmap abs arbitrary) `suchThat` (>0), (>0))

lessThan :: (Ord a, Arbitrary a, Show a) => a -> Predicate a
lessThan v = predicate (show v ++ " > ") (arbitrary `suchThat` (<v), (<v))

is :: (Eq a, Show a) => a -> Predicate a
is a = predicate ("is "++(show a)) (return a,(a==))

permutationOf :: (Ord a, Show a) => [a] -> Predicate [a]
permutationOf bs = predicate ("permutationOf " ++ (show bs)) (shuffle bs, (((sort bs) ==) . sort))

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

-- conjunction over a list
andl :: Predicate a -> Predicate [a]
andl (g, p) = (replicateM g, test)           
    where
        test []     = Nothing
        test (x:xs) = case p x of
                        Nothing -> test xs
                        Just s  -> Just s

wildcard :: (Arbitrary a) => Predicate a
wildcard = (arbitrary, const Nothing)
