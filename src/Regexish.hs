module Regexish where
import Text.Regex.Posix
import Test.QuickCheck
import Control.Monad

data Regexish = Match String
              | AnyNumberOf Regexish
              | Anything
              | Choice Regexish Regexish
              | Sequence Regexish Regexish

genStr :: Gen String
genStr = listOf $ oneof $ fmap return "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789"

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
          l <- gen (n `div` 2)
          r <- gen (n `div` 2)
          return $ Choice l r,
        do
          l <- gen (n `div` 2)
          r <- gen (n `div` 2)
          return $ Sequence l r]

instance Show Regexish where
  show = toString

(>*>) = Sequence
(<||>) = Choice

paren s = "(" ++ s ++ ")"

toString :: Regexish -> String
toString (Match s)       = s
toString (AnyNumberOf s) = paren (toString s) ++ "*"
toString Anything        = ".*"
toString (Choice a b)    = paren $ toString a ++ "|" ++ toString b
toString (Sequence a b)  = toString a ++ toString b

matches :: Regexish -> String -> Bool
matches rgx s = s =~ toString rgx

genRegex :: Regexish -> Gen String
genRegex (Match s) = return s
genRegex (AnyNumberOf rgx) = do
  i <- fmap abs arbitrary
  fmap concat $ replicateM i (genRegex rgx)
genRegex Anything = genStr
genRegex (Choice a b) = oneof [genRegex a, genRegex b]
genRegex (Sequence a b) = do
  as <- genRegex a
  bs <- genRegex b
  return $ as ++ bs
