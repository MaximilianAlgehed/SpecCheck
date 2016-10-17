-module(erlangPOPLExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() -> register(p, self()),
       receive
           Hs -> protocol(Hs)
       end.

protocol(Hs) -> 
         Hs ! {pure, [1.0,2.0,3.0]},
         receive
             {_, {choice, execute}} -> execute();
             {_, {choice, continue}} -> continue(Hs)
         end.

execute() ->
        receive
            {Hs, {pure, Actions}} -> Hs ! {pure, status(Actions)},
                                     continue(Hs)
        end.

% simulating lots of nasty side-effects!!!
status([]) -> [];
status([{output, 1, _}|TL]) -> [{out, 0, true}|status(TL)];
status([{output, I, J}|TL]) when J >= 10 -> [{out, I, false}|status(TL)];
status([{output, I, J}|TL]) when J < 10 -> [{out, I, true}|status(TL)];
status([{input, I}|TL]) -> [{inp, I, 5.0}|status(TL)].

continue(Hs) -> Hs ! {choice, stop}.
