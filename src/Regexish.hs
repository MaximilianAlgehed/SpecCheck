module Regexish where
import Text.Regex.Posix
import Test.QuickCheck
import Control.Monad
import Data.List
import Predicate

data Regexish = Match String
              | AnyNumberOf Regexish
              | Anything
              | Choice [Regexish]
              | Sequence Regexish Regexish

alnum :: String
alnum = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789"

genStr :: Gen String
genStr = listOf $ oneof $ fmap return (alnum ++ " ")

instance Arbitrary Regexish where
  arbitrary = sized gen
    where
      gen 0 = oneof [
          do
            s <- genStr
            return (Match s),
          return Anything
          ]
      gen n = frequency [
        (10, gen 0),
        (1, do
          s <- gen $ (round . sqrt . fromInteger . toInteger) n
          return $ AnyNumberOf s),
        (10, do
          i <- fmap (abs . (flip mod n) . abs) arbitrary
          xs <- replicateM (i + 1) $ gen (n `div` (i + 1))
          return $ Choice xs),
        (10, do
          l <- gen (n `div` 2)
          r <- gen (n `div` 2)
          return $ Sequence l r)]

instance Show Regexish where
  show = toString

(>*>)  = Sequence

paren s = "(" ++ s ++ ")"

toString, toString' :: Regexish -> String
toString rgx = "^" ++ toString' rgx ++ "$"
toString' (Match s)       = s
toString' (AnyNumberOf s) = paren (toString' s) ++ "*"
toString' Anything        = ".*"
toString' (Choice xs)     = paren $ intercalate "|" [toString' x | x <- xs]
toString' (Sequence a b)  = toString' a ++ toString' b

matches :: Regexish -> String -> Bool
matches rgx s = s =~ toString rgx

genRegex :: Regexish -> Gen String
genRegex (Match s)         = return s
genRegex (AnyNumberOf rgx) = do
  i <- fmap abs arbitrary
  fmap concat $ replicateM i (genRegex rgx)
genRegex Anything          = genStr
genRegex (Choice xs)       = oneof [genRegex x | x <- xs]
genRegex (Sequence a b)    = do
  as <- genRegex a
  bs <- genRegex b
  return $ as ++ bs

regexP :: String -> Regexish -> Predicate String
regexP s rgx = predicate s (genRegex rgx, matches rgx)

-- Some syntax
integer :: Regexish
integer = (Choice [Match (show x) | x <- [1..9]]) >*> AnyNumberOf (Choice [Match (show x) | x <- [0..9]])

word :: Regexish
word = atLeastOne $ Choice [Match [x] | x <- alnum]

atLeastOne :: Regexish -> Regexish
atLeastOne rgx = rgx >*> AnyNumberOf rgx

x <|> y = Choice [x, y]

possibly :: Regexish -> Regexish
possibly rgx = Choice [Match "", rgx]

-- Tests
prop_genRegex_matches =
  forAll (arbitrary :: Gen Regexish) $ \rgx ->
  forAll (genRegex rgx) $ \s ->
  matches rgx s
