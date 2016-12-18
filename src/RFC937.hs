import Typeclasses
import Test.QuickCheck hiding (choose)
import ST
import Foreign.Erlang
import Predicate
import CSpec
import qualified Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Data.List
import Control.DeepSeq
import Data.Generics
import Regexish

{- The simple interpretation -}
heloMessage = regexP "HELO_message" $ Match "HELO " >*> word >*> Match " " >*> word 

loginStatus = regexP "Integer"      $ Match "#" >*> integer

quit = regexP "quit" $ Match "QUIT"

fold = regexP "fold" $ Match "FOLD " >*> word

read = regexP "read" $ Match "READ " >*> integer

rfc :: CSpec ErlType ()
rfc =
  do
    get greeting

    next <- send $ quit ||| heloMessage
    if matches (Match "QUIT") then
      stop
    else
      return ()
    get loginStatus

    next <- send $ quit ||| fold ||| read
    if matches (Match "QUIT") then
      stop
    else
      return ()
