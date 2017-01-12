{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Debug.Trace
import GHC.Generics hiding (from)
import Control.DeepSeq
import Control.Concurrent
import System.Process 
import Test.QuickCheck hiding (choose)
import ST
import Foreign.Erlang
import Predicate
import CSpec
import qualified Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.List

type ProductID     = Int
type Price         = Int 
data ShoppingState = ShoppingState {basket :: [ProductID],
                                    price  :: Price} deriving (Generic, NFData, Show)

initialShoppingState = ShoppingState {
                        basket = [],
                        price  = 0
                       }

anyBook :: Predicate ProductID
anyBook = posNum

buyBook :: CSpecS ShoppingState ErlType ()
buyBook =
  do
    book      <- send anyBook
    priceBook <- get posNum -- Get the price of the book 
    modify $ \st -> let books  = basket st
                        priceT = price st in st {basket = book:books, price = priceT + priceBook}

removeBook :: CSpecS ShoppingState ErlType ()
removeBook =
  do
    books       <- basket <$> state
    if null books then
        bug "bug_002" (return ()) $ void (send (is (-1 :: Int)))
    else
      do
        removedBook <- send (from books)
        modify $ \st -> st {basket = books \\ [removedBook]}

searchBooks :: CSpec ErlType ()
searchBooks =
  bug "bug_005"
  (do
    query <- send wildcard
    void $ get $ relevantResults query)
  (void $ get (is @[Int] []))

relevantResults :: String -> Predicate [(String, ProductID)]
relevantResults s = predicate ("relevantResults "++s)
  (do
    n <- arbitrary 
    replicateM n (relevantResult s),
   (\xs -> and [s `isInfixOf` (fst x) | x <- xs])
  )
  where
    relevantResult s = do
      b <- arbitrary 
      a <- arbitrary
      p <- arbitrary
      return $ (b ++ s ++ a, p)

getBasket :: CSpecS ShoppingState ErlType ()
getBasket = do
  s <- state
  bug "bug_001"
    (void $ get $ permutationOf (basket s) .*. is (price s))
    $ do
        bug "bug_004" (get $ permutationOf (basket s)) (get (is []))
        get $ is (price s)
        return ()

bookProtocol :: CSpecS ShoppingState ErlType ()
bookProtocol = do
  bug "bug_003" (return ()) (modify $ \st -> st {price = price st + 2}) -- We are having some serious bug issues at the moment
  forever $ do
    action <- choose ["finish", "buy", "basket", "remove", "search"]
    case action of
      "finish" -> stop
      "buy"    -> buyBook
      "basket" -> getBasket
      "remove" -> removeBook
      "search" -> searchBooks

main :: IO ()
main = do
  let bugs = ["bug_001", "bug_002", "bug_003", "bug_004", "bug_005"]
  coherentS bookProtocol initialShoppingState
  ph <- spawnCommand "erl -sname erl > /dev/null"
  threadDelay 2000000
  self <- createSelf "haskell@localhost"
  runErlangS self "bigExample" "main" bugs bookProtocol initialShoppingState
  callCommand "./kill-erlang-node.sh erl"
