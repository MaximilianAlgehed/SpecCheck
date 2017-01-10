-module(bigExample).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    _ -> loop([], 2)
  end.

loop(Lst, Price) ->
  receive 
    {_,  "finish"}  -> exit(done);
    {_,  "buy"}     -> buy(Lst, Price);
    {Hs, "basket"}  -> basket(Lst, Price, Hs);
    {_,  "remove"}  -> remove(Lst, Price);
    {Hs, "search"}  -> search(Lst, Price, Hs)
  end.

buy(Lst, Price) ->
  receive
    {Hs, Book} -> Hs ! price(Book),
                  loop(Lst, Price + price(Book))
  end.

basket(Lst, Price, Hs) ->
  Hs ! Lst,
  Hs ! Price,
  loop(Lst, Price).

remove(Lst, Price) ->
  receive
    {_, Book} -> loop(lists:filter(fun(B) -> B == Book end, Lst), Price)
  end.

search(Lst, Price, Hs) ->
  Hs ! [],
  loop(Lst, Price).

price(Book) -> 10*Book*Book.
