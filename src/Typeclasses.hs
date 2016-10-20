{-# LANGUAGE GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}
module Typeclasses where
import Foreign.Erlang
import Data.Char

class a :<: b where
    embed   :: a -> b
    extract :: b -> Maybe a

instance a :<: a where
    embed   = id
    extract = Just . id

instance Int :<: ErlType where
    embed x             
       | abs x <= 0x7FFFFFFF = ErlInt x        
       | otherwise           = ErlBigInt (fromIntegral x) -- Haskell Int (might) use 64 bits whether erlang's small Int use only 32 bit

    extract (ErlInt x)    = Just x
    extract (ErlBigInt x) = Just $ fromIntegral x
    extract _ = Nothing

instance Double :<: ErlType where
    embed   x            = ErlFloat x 
    extract (ErlFloat x) = Just x
    extract _ = Nothing

instance Float :<: ErlType where
    embed x              = ErlFloat (realToFrac x)
    extract (ErlFloat x) = Just $ realToFrac x
    extract _ = Nothing

instance Integer :<: ErlType where
    embed   x             = ErlBigInt x
    extract (ErlInt x)    = Just $ fromIntegral x
    extract (ErlBigInt x) = Just x
    extract _ = Nothing

instance String :<: ErlType where
    embed   x             = ErlString x
    extract ErlNull       = Just $ ""
    extract (ErlString x) = Just $ x
    extract (ErlAtom x)   = Just $ x
    extract (ErlList xs)  = Just $ map (chr . fromErlang) xs
    extract _ = Nothing

instance Bool :<: ErlType where
    embed   True              = ErlAtom "true"
    embed   False             = ErlAtom "false"
    extract (ErlAtom "true")  = Just True
    extract (ErlAtom "false") = Just False
    extract _ = Nothing

instance [ErlType] :<: ErlType where
    embed   []           = ErlNull
    embed   xs           = ErlList xs
    extract ErlNull      = Just []
    extract (ErlList xs) = Just xs
    extract _ = Nothing

instance (a :<: ErlType) => [a] :<: ErlType where
    embed   []           = ErlNull
    embed   xs           = ErlList . map embed $ xs
    extract ErlNull      = Just []
    extract (ErlList xs) = sequence $ map extract xs
    extract _ = Nothing

instance (a :<: ErlType, b :<: ErlType) => (a, b) :<: ErlType where
    embed   (x, y)            = ErlTuple [embed x, embed y]
    extract (ErlTuple [x, y]) = do
                                    l <- extract x
                                    r <- extract y
                                    return (l, r)
