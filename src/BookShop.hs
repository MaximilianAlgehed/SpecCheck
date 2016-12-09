import ST
import Foreign.Erlang
import CSpec
import Predicate

{- An example of buying books from amazon with monads -}
bookShop :: CSpecS [Int] ErlType ()
bookShop = loop

loop :: CSpecS [Int] ErlType ()
loop =
    do
        b <- send wildcard
        modify $ \books -> b:books

        choice <- choose ["another", "request"]
        case choice of
            "another" -> loop
            "request" -> request
            
request :: CSpecS [Int] ErlType ()
request =
    do
        books <- state
        get $ permutationOf books

        choice <- choose ["another", "done"]
        case choice of
            "another" -> loop
            "done"    -> stop

main :: IO ()
main = do
        putStrLn "Testing coherence..."
        coherentS bookShop []
        self <- createSelf "haskell@localhost"
        runErlangS self "erlangBooks" "main" bookShop []
