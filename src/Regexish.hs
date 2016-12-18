module Regexish where
import Text.Regex.Posix
import Test.QuickCheck
import Control.Monad
import Data.List

data Regexish = Match String
              | AnyNumberOf Regexish
              | Anything
              | Choice [Regexish]
              | Sequence Regexish Regexish

genStr :: Gen String
genStr = listOf $ oneof $ fmap return "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789.:;-"

instance Arbitrary Regexish where
  arbitrary = sized gen
    where
      gen 0 = oneof [
          do
            s <- genStr
            return (Match s),
          return Anything
          ]
      gen n = oneof [
        gen 0,
        do
          s <- gen (n-1)
          return $ AnyNumberOf s,
        do
          i <- fmap (abs . (flip mod n) . abs) arbitrary
          xs <- replicateM (i + 1) $ gen (n `div` (i + 1))
          return $ Choice xs,
        do
          l <- gen (n `div` 2)
          r <- gen (n `div` 2)
          return $ Sequence l r]

instance Show Regexish where
  show = toString

(>*>)  = Sequence

paren s = "(" ++ s ++ ")"

toString :: Regexish -> String
toString (Match s)       = s
toString (AnyNumberOf s) = paren (toString s) ++ "*"
toString Anything        = ".*"
toString (Choice xs)     = paren $ intercalate "|" [toString x | x <- xs]
toString (Sequence a b)  = toString a ++ toString b

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

-- Some syntax
integer :: Regexish
integer = (Choice [Match (show x) | x <- [1..9]]) >*> AnyNumberOf (Choice [Match (show x) | x <- [0..9]])

perhaps :: Regexish -> Regexish
perhaps rgx = Choice [Match "", rgx]

floating :: Regexish
floating = integer >*> perhaps (Match "." >*> integer)
