import ST
import Foreign.Erlang
import CSpec
import Predicate

{- An example of buying books from amazon with monads -}
bookShop :: CSpec ErlType ()
bookShop = loop []

loop :: [Int] -> CSpec ErlType ()
loop books =
    do
        b <- send wildcard
        let bs = b:books

        choice <- choose ["another", "request"]
        case choice of
            "another" -> loop bs
            "request" -> request bs
            
request :: [Int] -> CSpec ErlType ()
request bs =
    do
        get $ permutationOf bs

        choice <- choose ["another", "done"]
        case choice of
            "another" -> loop bs
            "done"    -> stop

main :: IO ()
main = do
        self <- createSelf "haskell@localhost"
        runErlang self "erlangBooks" "main" bookShop
