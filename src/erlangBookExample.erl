-module(erlangBookExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive 
            Hs -> loop([])
       end.

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
        {_, {choice, another}} -> loop(Lst);
        {Hs, {choice, request}} -> Hs ! {pure, Lst}, finish(Lst)
    end.

finish(XS) ->
    receive
        {_, {choice, another}} -> loop(XS);
        {_, {choice, done}} -> exit(done)
    end.
