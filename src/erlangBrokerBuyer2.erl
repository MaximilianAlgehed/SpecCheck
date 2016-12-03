-module(erlangBrokerBuyer2).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive 
            _ -> loop(60)
       end.

loop(P) ->
    receive 
        {Hs, {pure, RequestedPrice}} -> if
                                            RequestedPrice < P  -> Hs ! {choice, response},
                                                                   NewPrice = RequestedPrice + ((P - RequestedPrice) / 2),
                                                                   % Basic recalculation of the price
                                                                   Hs ! {pure, NewPrice},
                                                                   loop(NewPrice);
                                            RequestedPrice >= P -> Hs ! {choice, response},
                                                                   Hs ! {pure, RequestedPrice-0.1},
                                                                   exit(done)
                                        end
    end.
