import ST
import CSpec
import Predicate
import Foreign.Erlang

-- There are two bugs in the erlang implementation,
-- some times the price goes up, due to not updating
-- the price on the erlang side, and if the initial
-- price is set to something > 10 on the client side
-- there might be a type error as the server just
-- accepts a price rather than sending the "response"
-- message to the client

buyer :: CSpecS Double ErlType ()
buyer =
    do
        price  <- state
        reqP   <- send $ inRange (0, price+0.1)
        br     <- branch ["response", "fault"]
        case br of
            "response" -> do
                            brokerPrice <- get $ inRange (reqP-0.1, price+0.1) 
                            continue brokerPrice
            "fault"    -> stop

continue :: Double -> CSpecS Double ErlType ()
continue brokerPrice =
    do
        br <- branch ["accept", "request"]
        case br of
            "accept"  -> stop
            "request" -> do
                            store brokerPrice
                            buyer

main = do
    putStrLn "Testing coherence..."
    coherentS buyer 100
    self <- createSelf "haskell@localhost"
    runErlangS self "erlangBrokerBuyer" "main" buyer 100
