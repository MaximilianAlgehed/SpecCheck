{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module SimpleErlangExample where

import Debug.Trace
import Data.List
import Control.Concurrent
import Control.Concurrent.Chan
import Typeclasses
import Model
import Test.QuickCheck
import SessionTypes hiding (B, L, R)
import JSONType
import LTL hiding (check)
import Run
import Erlang
import Foreign.Erlang

-- | The universe of our types
data TypeUniverse = Int | IntPair deriving (Show, Eq)

-- | Mechanical instance, more or less
instance Implements ErlType TypeUniverse where
    implement Int = fmap ErlInt arbitrary
    implement IntPair = fmap ErlTuple $ sequence $ map (fmap ErlInt) [arbitrary, arbitrary]

-- | Almost mechanical instance
instance Checks TypeUniverse ErlType where
    check Int (ErlInt _) = True
    check IntPair (ErlTuple [ErlInt _, ErlInt _]) = True
    check _ _ = False
    
-- | The very simple session type
simpleSessionType :: SessionType TypeUniverse
simpleSessionType = (!) Int :. (?) IntPair :. end

simplePredicate :: LTL (Interaction (Protocol ErlType))
simplePredicate = Atomic (\(Sent (Pure (ErlInt i))) ->
                           X $ Atomic
                                 (\(Got (Pure (ErlTuple [ErlInt x, ErlInt y]))) ->
                                   fromBool $ (x == i) && (y == i*2)
                                 )
                         )

testSimpleErlang = runErlang "simpleErlangExample" "main" simpleSessionType simplePredicate
