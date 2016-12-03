-module(erlangBrokerBuyer).
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
                                                                   Hs ! {choice, request},
                                                                   loop(NewPrice);
                                            RequestedPrice >= P -> Hs ! {choice, response},
                                                                   Hs ! {pure, RequestedPrice},
                                                                   Hs ! {choice, accept},
                                                                   exit(done)
                                        end
    end.
