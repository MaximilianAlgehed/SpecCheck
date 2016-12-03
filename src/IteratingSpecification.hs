import ST
import Predicate
import Foreign.Erlang

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
