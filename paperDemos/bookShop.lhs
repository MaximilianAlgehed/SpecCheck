> import Control.Concurrent
> import System.Process 
> import ST
> import Foreign.Erlang
> import CSpec
> import Predicate

In this example we describe a protocol for ordering books from some service.
We begin by ordering a book, we may then choose to either order another book
or request to see our basket.

> bookShop :: SpecS [Int] ErlType ()
> bookShop = do 
>   b <- send anything 
>   modify $ \books -> b:books
> 
>   choice <- choose ["another", "request"]
>   case choice of
>     "another" -> bookShop
>     "request" -> request

When we request to see our basket the other party should send us a list of the books
we have ordered so far.

> request :: SpecS [Int] ErlType ()
> request = do
>   books <- state
>   get $ permutationOf books
> 
>   choice <- choose ["another", "done"]
>   case choice of
>     "another" -> bookShop 
>     "done"    -> stop

We can test an erlang implementation of the server in our protocol.

> main :: IO ()
> main = do

We begin by making sure our protocol is coherent

>   putStrLn "Testing coherence..."
>   coherentS bookShop []

We then compile the erlang code we are testing and start an erlang node
to run the code in question

>   callCommand "erlc erlangBooks.erl"
>   ph <- spawnCommand "erl -sname erl > /dev/null"
>   threadDelay 2000000

We then spawn our erlang node, from haskell, and start testing

>   putStrLn "\nTesting protocol compliance..."
>   self <- createSelf "haskell@localhost"
>   runErlangS self "erlangBooks" "main" [] bookShop []
>   callCommand "./kill-erlang-node.sh erl"
>   callCommand "rm erlangBooks.beam"

Testing produces the following output,
as we can see there is a bug in the implementation
and we are presented with a minimal failing counterexample.

< Failed after 0 tests
< With: permutationOf [5,-19] [5]
< In:
< ---
<     Sent: -19
<     Sent: "another"
<     Sent: 5
<     Sent: "request"
<     Got:  [5]
< ---
< 
< ~~~~~ after shrinking
<
< With: permutationOf [-1] []      
< In:
< ---
<     Sent: -1
<     Sent: "request"
<     Got:  []
< ---
