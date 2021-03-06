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

{- The simple interpretation -}
helom    = "HELO " >*> word >*> " " >*> word 
nnnm     = "#"     >*> integer
cccm     = "="     >*> integer
quitm    = "QUIT"
foldm    = "FOLD " >*> word
readm    = "READ"  >*> possibly (" " >*> integer)
retrm    = "RETR"
greeting = "POP2 " >*> Anything
ackdm    = "ACKD"
acksm    = "ACKS"

heloP     = regexP "HELO_message" helom 
nnnmP     = regexP "nnn"          nnnm
cccmP     = regexP "ccc"          cccm
quitP     = regexP "quit"         quitm
foldP     = regexP "fold"         foldm
readP     = regexP "read"         readm
retrP     = regexP "retr"         retrm 
greetingP = regexP "greeting"     greeting
ackdP     = regexP "ackd"         ackdm
acksP     = regexP "acks"         acksm 

-- Simplified POP2 protocol

call :: CSpecS Int String ()
call = do
  get greetingP

  next <- send $ quitP ||| heloP
  transition next [
    (quitm,  stop),
    (helom,  nmbr)]

nmbr :: CSpecS Int String ()
nmbr = do
  numMsgs <- get nnnmP

  next    <- send $ quitP ||| foldP ||| readP
  transition next [
    (quitm, stop),
    (readm, size next),
    (foldm, nmbr)]

size :: String -> CSpecS Int String ()
size trans = do
  case trans of
    "READ" -> return ()
    "ACKD" -> modify (+1)
    "ACKS" -> modify (+1)
    xs     -> store (read $ drop 5 xs) 

  rep   <- get cccmP
  let sz = read (tail rep)

  next <- send $ quitP ||| foldP ||| readP ||| retrP
  transition next [
    (quitm, stop),
    (foldm, nmbr),
    (readm, size next),
    (retrm, xfer sz)]

xfer :: Int -> CSpecS Int String ()
xfer sz = do
  get $ predicate ("hasSize "++(show sz)) (replicateM sz arbitrary, \xs -> length (xs :: String) == sz)
  ack <- send $ ackdP ||| acksP
  size ack

rfc :: CSpecS Int String ()
rfc = call
