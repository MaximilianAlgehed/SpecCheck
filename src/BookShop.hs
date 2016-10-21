import ST
import Foreign.Erlang

{- An example of "buying books from amazon" -}
bookShop :: ST ErlType
bookShop = bookShop' ([] :: [Int])

bookShop' :: [Int] -> ST ErlType
bookShop' bs =
    Send wildcard $ \b ->
    let bs' = b:bs in
    ("another", bookShop' bs') <|> ("request", Get (isPermutation bs') cont)

cont :: [Int] -> ST ErlType
cont bs = ("another", bookShop' bs) <|> ("done", End)