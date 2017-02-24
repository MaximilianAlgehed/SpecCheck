{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
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

{- TODO:
 -   * Keep track of deleted messages
 -   * Limit the integers to some sane range
 -}

{- POP3 -}
okay     = regexP "okay"     $ "+OK" >*> possibly (" " >*> Anything)
nokay    = regexP "nokay"    $ "-ERR" >*> possibly (" " >*> Anything)
quit     = regexP "quit"     $ "QUIT"
stat     = regexP "stat"     $ "STAT"
maildrop = regexP "maildrop" $ "+OK " >*> integer >*> " " >*> integer
list     = regexP "list"     $ "LIST" >*> possibly (" " >*> integer)
retr     = regexP "retr"     $ "RETR " >*> integer
dele     = regexP "dele"     $ "DELE " >*> integer
noop     = regexP "noop"     $ "NOOP"
rset     = regexP "rset"     $ "RSET"
user     = regexP "user"     $ "USER " >*> Anything -- This is not a good way to do it in general
pass     = regexP "pass"     $ "PASS " >*> Anything -- This is also not a good way to do it in general

auth :: Spec String ()
auth = do
  get okay 
  login 
  transaction

login :: Spec String ()
login = do
  send user
  s <- get $ okay ||| nokay
  if okay $$ s then
    tryPassword
  else
    login 

tryPassword :: Spec String ()
tryPassword = do
  send pass
  s <- get $ okay ||| nokay
  if okay $$ s then
    return ()
  else
    login 

transaction :: Spec String ()
transaction = do
  sent <- send $ stat ||| maildrop ||| list ||| retr ||| dele ||| noop ||| rset ||| quit
  get $ okay ||| nokay
  if quit $$ sent then
    update
  else
    transaction

update :: Spec String ()
update = do
  send quit
  get $ okay ||| nokay
  return ()
