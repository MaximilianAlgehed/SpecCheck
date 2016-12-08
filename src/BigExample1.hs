import ST
import Foreign.Erlang
import Predicate
import CSpec
import qualified Control.Monad.Trans.State as S
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

getPrice :: ProductID -> CSpecS ShoppingState ErlType Double
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
bookProtocol = loop

loop :: CSpecS ShoppingState ErlType ()
loop =
    do
        action <- choose ["finish", "buy", "basket"]
        basket <- basket <$> state
        case action of
            "finish" -> stop
            "buy"    -> buyBook
            "basket" -> do
                           basket <- get $ permutationOf (fst basket) .*. is (snd basket)
        loop

main = do
        coherentT (dual bookProtocol) (\m -> S.evalStateT m initialShoppingState)
