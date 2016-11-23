import ST
import Foreign.Erlang

-- There are two bugs in the erlang implementation,
-- some times the price goes up, due to not updating
-- the price on the erlang side, and if the initial
-- price is set to something > 10 on the client side
-- there might be a type error as the server just
-- accepts a price rather than sending the "response"
-- message to the client

buyer :: ST ErlType
buyer = buyer' 100 -- Set 100 as the maximum price, for example purposes

buyer' :: Double -> ST ErlType
buyer' price = Send (inRange (0, price)) $ \ reqP -> 
               ("response", Get (inRange (reqP, price)) continue) <&> ("fault", End)

continue :: Double -> ST ErlType
continue brokerPrice = ("accept", End) <&> ("request", buyer' brokerPrice)
