{-# LANGUAGE RankNTypes #-}
import ST
import Foreign.Erlang
import Predicate
import CSpec
import qualified Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont

type ProductID = Int
type Price     = Double
type Basket    = ([ProductID], Price)
data ShoppingState = ShoppingState {basket :: Basket}

initialShoppingState = ShoppingState {
                        basket = ([], 0)
                       }

anyBook :: Predicate ProductID
anyBook = posNum

getPrice :: ProductID -> CSpec ErlType Double
getPrice productID =
    do
        send (is productID)
        get posNum

buyBook :: CSpecS ShoppingState ErlType ()
buyBook =
    do
        book  <- send anyBook
        price <- getPrice book
        modify $ \st -> let (books, priceT) = basket st in st {basket = (book:books, priceT + price)}

bookProtocol :: CSpecS ShoppingState ErlType ()
bookProtocol =
    do
        action <- choose ["finish", "buy", "basket"]
        basket <- basket <$> state
        case action of
            "finish" -> stop
            "buy"    -> buyBook
            "basket" -> void $ get $ permutationOf (fst basket) .*. is (snd basket)
        bookProtocol

main = do
        coherentS (dual bookProtocol) initialShoppingState
