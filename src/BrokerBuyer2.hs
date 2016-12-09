import ST
import CSpec
import Predicate
import Foreign.Erlang

buyer :: CSpecS Double ErlType ()
buyer =
    do
        price  <- state
        reqP   <- send $ inRange (0, price+0.1)
        br     <- branch ["response", "fault"]
        case br of
            "response" -> do
                            brokerPrice <- get $ inRange (reqP-0.1, price+0.1) 
                            if brokerPrice <= reqP then
                                stop
                            else
                                do
                                    store brokerPrice
                                    buyer
            "fault"    -> stop

main = do
    putStrLn "Testing coherence..."
    coherentS buyer 100
    self <- createSelf "haskell@localhost"
    runErlangS self "erlangBrokerBuyer2" "main" buyer 100
