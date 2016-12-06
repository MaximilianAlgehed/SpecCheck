import ST
import Foreign.Erlang
import Predicate
import CSpec

type ProductID = Int
type Price     = Double
type Basket    = ([ProductID], Price)

addToBasket :: ProductID -> Price -> Basket -> Basket
addToBasket id price (ids, priceT) = (id:ids, priceT + price)

emptyBasket :: Basket
emptyBasket = ([], 0)

anyBook :: Predicate ProductID
anyBook = posNum

getPrice :: ProductID -> CSpec ErlType Double
getPrice productID =
    do
        send (is productID)
        get posNum

buyBook :: Basket -> CSpec ErlType Basket
buyBook basket =
    do
        book  <- send anyBook
        price <- getPrice book
        return $ addToBasket book price basket

bookProtocol :: CSpec ErlType ()
bookProtocol = bookProtocol' emptyBasket

bookProtocol' :: Basket -> CSpec ErlType ()
bookProtocol' basket =
    do
        action <- choose ["finish", "buy", "basket"]
        basket <- act basket action
        bookProtocol' basket

act _      "finish" = stop
act basket "buy"    = buyBook basket
act basket "basket" = get (permutationOf (fst basket) .*. is (snd basket))
