-module(erlangBookExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       loop([]).

loop(XS) ->
    receive 
        {_, {pure, I}} -> continue(I, XS)
    end.

continue(I, XS) ->
    Lst = if
            I < 0 -> XS;
            true  -> [I|XS]
          end,
    receive 
        {_, chooseLeft} -> loop(Lst);
        {_, chooseRight} -> finish(Lst)
    end.

finish(XS) ->
    receive 
        {Hs, {pure, requestBooks}} -> Hs ! {pure, XS}
    end,
    receive
        {_, chooseLeft} -> loop(XS);
        {_, chooseRight} -> exit(done)
    end.
