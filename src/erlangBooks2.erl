-module(erlangBooks2).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive 
            _ -> loop([])
       end.

loop(XS) ->
    receive 
        {_, {pure, I}} -> continue(I, XS)
    end.

continue(I, XS) ->
    Lst = if
            I < 0 -> XS;
            length(XS) >= 5 -> XS;
            true  -> [I|XS]
          end,
    receive 
        {_, {pure, "another"}} -> loop(Lst);
        {Hs, {pure, "request"}} -> Hs ! {pure, Lst}, finish(Lst)
    end.

finish(XS) ->
    receive
        {_, {pure, "another"}} -> loop(XS);
        {_, {pure, "done"}} -> exit(done)
    end.
