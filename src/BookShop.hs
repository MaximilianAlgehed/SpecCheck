import Test.QuickCheck
import ST
import Foreign.Erlang

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

bookShop' :: [Int] -> ST ErlType
bookShop' bs =
    Send wildcard $ \b ->
    let bs' = b:bs in
    Choose
        (frequency [(1, return 0), (1, return 1)])
        [("another", bookShop' bs'), ("request", Get (permutationOf bs') cont)]

cont :: [Int] -> ST ErlType
cont bs = ("another", bookShop' bs) <|> ("done", End)

book :: Predicate Int
book = wildcard
