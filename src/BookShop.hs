import ST
import Foreign.Erlang

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

bookShop' :: [Int] -> ST ErlType
bookShop' bs =
    Send book $ \b ->
    let bs' = b:bs in
    ("another", bookShop' bs') <|> ("request", Get (permutationOf bs') cont)

cont :: [Int] -> ST ErlType
cont bs = ("another", bookShop' bs) <|> ("done", End)

book :: Predicate Int
book = wildcard
