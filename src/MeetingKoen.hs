import ST
import Predicate
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
book = wildcard -- posNum

{- Two parties deciding on a price -}
buyer :: ST ErlType
buyer = buyer' 100 -- Set 100 as the maximum price, for example purposes

buyer' :: Double -> ST ErlType
buyer' price = Send (inRange (0, price)) $ \ reqP -> 
               ("response", Get (inRange (reqP, price)) continue) <&> ("fault", End)

continue :: Double -> ST ErlType
continue brokerPrice = ("accept", End) <&> ("request", buyer' brokerPrice)

{- The process of iterating a specification, to show that "checkCoherence" can be useful -}
type Message = Int

validMessage :: Predicate Message
validMessage = posNum

-- Send any number of messages
protocol :: ST Int
protocol = Send validMessage .- ("another", protocol) <|> ("finish", End)

-- Tell the other party how many messages will be sent
protocol2 :: ST Int
protocol2 = Send (posNum :: Predicate Int) .- protocol

-- Send precisely the number of messages that you said you would
protocol3 :: ST Int
protocol3 = Send posNum $ \n -> foldr (.-) End (replicate n (Send validMessage))

-- Messages of a certain size
type Message' = (Int, Message)

validMessageInSizeRange :: (Int, Int) -> Predicate Message'
validMessageInSizeRange r = inRange r .*. validMessage

-- This specification is WRONG on purpose!
protocol4 :: ST ErlType
protocol4 = Send posNum $ \n -> Send (validMessageInSizeRange (1, n)) (messages n)

messages :: Int -> Message' -> ST ErlType
messages 0 _ = End
messages n m = Send (validMessageInSizeRange (1, n - (fst m))) (messages (n - (fst m)))
