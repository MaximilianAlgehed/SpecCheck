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

login :: CSpec ErlType Integer 
login =
  do
    send heloMessage
    s <- get loginStatus
    return $ read (tail s)

call :: CSpec ErlType ()
call =
  do
    get greeting
    next <- send $ Match "QUIT" <|> (Match "FOLD " >*> word)
    if matches (Match "QUIT") then
      stop
    else
      folder 

folder :: CSpec ErlType ()
