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

addToBasket :: (Monad m) => ProductID -> Price -> S.StateT ShoppingState m ()
addToBasket id price =
    S.modify $ \st -> let (ids, priceT) = basket st in st {basket = (id:ids, priceT + price)}

initialShoppingState = ShoppingState {
                        basket = ([], 0)
                       }

anyBook :: Predicate ProductID
anyBook = posNum

getPrice :: ProductID -> CSpecT (S.StateT ShoppingState) ErlType Double
getPrice productID =
    do
        send (is productID)
        get posNum

buyBook :: CSpecT (S.StateT ShoppingState) ErlType ()
buyBook =
    do
        book  <- send anyBook
        price <- getPrice book
        lift $ addToBasket book price

bookProtocol :: CSpecT (S.StateT ShoppingState) ErlType ()
bookProtocol = loop

loop :: CSpecT (S.StateT ShoppingState) ErlType ()
loop =
    do
        action <- choose ["finish", "buy", "basket"]
        basket <- fmap basket $ lift $ S.get
        case action of
          "finish" -> stop
          "buy"    -> buyBook
          "basket" -> do
                         basket <- get (permutationOf (fst basket) .*. is (snd basket))
                         lift $ S.modify $ \st -> st {basket = basket}
        loop

main = do
        coherentT (dual bookProtocol) (\m -> S.evalStateT m initialShoppingState)
