-module(simpleErlangExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
        register(p, self()),
        receive
            %{Hs, {pure, 0}} -> Hs ! {pure, {0, 1}};
            {Hs, {pure, I}} -> Hs ! {pure, {I, 2*I}}
        end,
        exit(done).
