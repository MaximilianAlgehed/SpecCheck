-module(erlangBrokerBuyer).
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
                                                                   Hs ! "request",
                                                                   loop(NewPrice);      %Replacte with P for bug

                                            RequestedPrice >= P -> Hs ! "response",     %Comment these lines out for bug
                                                                   Hs ! RequestedPrice, %.............
                                                                   Hs ! "accept",
                                                                   exit(done)
                                        end
    end.
