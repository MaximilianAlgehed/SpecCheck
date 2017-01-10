module Predicate where
import Test.QuickCheck
import Control.Monad
import Data.List

data Predicate a = Predicate {generator :: Gen a, predf :: a -> Maybe String, name :: String, shrinker :: Maybe (a -> [a])}

predicate :: String -> (Gen a, (a -> Bool)) -> Predicate a
predicate s (g, p) = Predicate g (\a -> guard (not (p a)) >> return s) s Nothing

predicateShrink :: String -> (Gen a, (a -> Bool), a -> [a]) -> Predicate a
predicateShrink s (g, p, shr) = (predicate s (g, p)) {shrinker = Just shr}

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
(Predicate lg l ln ls) ||| (Predicate rg r rn rs) = Predicate (oneof [lg, rg]) (disj l r) (ln ++ "|||" ++ rn) shr
    where
        disj l r a = do
                        sl <- l a
                        sr <- r a
                        return $ "("++ sl ++ " || " ++ sr ++ ")"
        shr = do
          lhs <- ls
          rhs <- rs
          return $ \a -> lhs a ++ rhs a

(&&&) :: Predicate a -> Predicate a -> Predicate a
(Predicate lg l ln ls) &&& (Predicate rg r rn rs) = Predicate (oneof [lg, rg] `suchThat` ((== Nothing) . p)) p (ln ++ "&&&" ++ rn) shr
    where
        p a = case r a of
                Nothing -> case l a of
                            Nothing -> Nothing
                            Just s  -> Just s
                Just s  -> Just s
        shr = do
          lhs <- ls
          rhs <- rs
          return $ \a -> filter ((== Nothing) . p) (lhs a ++ rhs a)

(.*.) :: Predicate a -> Predicate b -> Predicate (a, b)
(Predicate lg l ln ls) .*. (Predicate rg r rn rs) = Predicate (do
                                                            l <- lg
                                                            r <- rg
                                                            return (l, r))
                                                        p
                                                        (ln ++ " .*. " ++ rn)
                                                        shr
    where
        p (a, b) = case r b of
                        Nothing -> case l a of
                                    Nothing -> Nothing
                                    Just s  -> Just s
                        Just s  -> Just s
        shr = do
          lhs <- ls
          rhs <- rs
          return $ \(a, b) -> [(a', b') | a' <- lhs a, b' <- rhs b]

wildcard :: (Arbitrary a) => Predicate a
wildcard = Predicate arbitrary (const Nothing) "_" Nothing

-- | `from xs` creates a predicate matching `\x -> elem x xs`
-- the predicate does shrinking by removing the selected element
-- from the list and reconsidering
from :: (Eq a, Show a) => [a] -> Predicate a
from xs = predicateShrink ("from " ++ show xs) (oneof (map return xs), \x -> x `elem` xs, const xs)

fromFreq :: (Eq a, Show a) => [(Int, a)] -> Predicate a
fromFreq xs = predicateShrink ("fromFreq " ++ show xs) (oneof (map (return . snd) xs), \x -> x `elem` (map snd xs), const (unFreq xs))
  where
    unFreq = concat . map (uncurry replicate) 
