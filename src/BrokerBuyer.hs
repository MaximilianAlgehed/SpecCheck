import ST
import Foreign.Erlang

buyer :: ST ErlType
buyer = buyer' (1/0) -- Infinity hack

buyer' :: Double -> ST ErlType
buyer' price = Send (inRange (0, price)) $ \ reqP -> 
               ("response", Get (inRange (reqP, price)) continue) <&> ("fault", End)

continue :: Double -> ST ErlType
continue brokerPrice = ("accept", End) <&> ("request", buyer' brokerPrice)
