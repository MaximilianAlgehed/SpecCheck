import Control.Concurrent
import System.Process 
import ST
import Foreign.Erlang
import CSpec
import Predicate

bookShop :: SpecS [Int] ErlType ()
bookShop = do 
  b <- send anything 
  modify $ \books -> b:books

  choice <- choose ["another", "request"]
  case choice of
    "another" -> bookShop
    "request" -> request
            
request :: SpecS [Int] ErlType ()
request = do
  books <- state
  get $ permutationOf books

  choice <- choose ["another", "done"]
  case choice of
    "another" -> bookShop 
    "done"    -> stop

main :: IO ()
main = do
  putStrLn "Testing coherence..."
  coherentS bookShop []
  ph <- spawnCommand "erl -sname erl > /dev/null"
  threadDelay 2000000
  putStrLn "\nTesting protocol compliance..."
  self <- createSelf "haskell@localhost"
  runErlangS self "erlangBooks" "main" [] bookShop []
  callCommand "./kill-erlang-node.sh erl"
