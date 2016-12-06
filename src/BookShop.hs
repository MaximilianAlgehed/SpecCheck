import ST
import Foreign.Erlang
import CSpec
import Predicate

{- An example of buying books from amazon with monads -}
bookShopCSpec :: CSpec ErlType ()
bookShopCSpec = bookShopCSpec' []

bookShopCSpec' :: [Int] -> CSpec ErlType ()
bookShopCSpec' books =
    do
        b <- send posNum
        let bs = b:books

        choice <- choose ["another", "request"]
        case choice of
            "another" -> bookShopCSpec' bs
            "request" -> request bs
            
request :: [Int] -> CSpec ErlType ()
request bs =
    do
        get $ permutationOf bs

        choice <- choose ["another", "done"]
        case choice of
            "another" -> bookShopCSpec' bs
            "done"    -> stop
