{-# LANGUAGE RankNTypes #-}
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

type ProductID = Int
type Price     = Double
data ShoppingState = ShoppingState {basket :: [ProductID],
                                    price  :: Price}

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
        return ()
    else
      do
        removedBook <- send (from books)
        modify $ \st -> st {basket = books \\ [removedBook]}

searchBooks :: CSpec ErlType ()
searchBooks =
  do
    query <- send wildcard
    void $ get $ relevantResults query

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
  s   <- state
  void $ get $ permutationOf (basket s) .*. is (price s)

bookProtocol :: CSpecS ShoppingState ErlType ()
bookProtocol = forever $ do
  action <- choose ["finish", "buy", "basket", "remove", "search"]
  case action of
    "finish" -> stop
    "buy"    -> buyBook
    "basket" -> getBasket
    "remove" -> removeBook
    "search" -> searchBooks

main = do
  coherentS bookProtocol initialShoppingState
  ph <- spawnCommand "erl -sname erl > /dev/null"
  threadDelay 2000000
  self <- createSelf "haskell@localhost"
  runErlangS self "bigExample" "main" bookProtocol initialShoppingState
  callCommand "./kill-erlang-node.sh erl"
