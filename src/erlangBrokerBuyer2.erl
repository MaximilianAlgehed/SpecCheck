-module(erlangBrokerBuyer2).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive 
           _ -> loop(60)
       end.

loop(P) ->
    receive 
        {Hs, RequestedPrice} -> if
                                    RequestedPrice < P  -> Hs ! "response",
                                                           NewPrice = RequestedPrice + ((P - RequestedPrice) / 2),
                                                           % Basic recalculation of the price
                                                           Hs ! NewPrice,
                                                           loop(NewPrice);
                                    RequestedPrice >= P -> Hs ! "response",
                                                           Hs ! RequestedPrice-0.1,
                                                           exit(done)
                                end
    end.
