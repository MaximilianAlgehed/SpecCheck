-module(erlangBrokerBuyer).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive 
            _ -> loop(100)
       end.

loop(P) ->
    receive 
        {Hs, {pure, RequestedPrice}} -> if
                                            RequestedPrice < P  -> Hs ! {choice, response},

                                                                   % Basic recalculation of the price
                                                                   Hs ! {pure, RequestedPrice + (P - RequestedPrice) / 2},
                                                                   Hs ! {choice, request},
                                                                   loop(P);
                                            RequestedPrice >= P -> Hs ! {choice, accept},
                                                                   exit(done)
                                        end
    end.
