{-# LANGUAGE RankNTypes #-}
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
helom    = Match "HELO " >*> word >*> Match " " >*> word 
nnnm     = Match "#" >*> integer
cccm     = Match "=" >*> integer
quitm    = Match "QUIT"
foldm    = Match "FOLD " >*> word
readm    = Match "READ" >*> possibly (Match " " >*> integer)
retrm    = Match "RETR"
greeting = Match "POP2 " >*> Anything

heloP     = regexP "HELO_message" $ helom 
nnnmP     = regexP "Integer"      $ nnnm
cccmP     = regexP "Integer"      $ cccm
quitP     = regexP "quit"         $ quitm
foldP     = regexP "fold"         $ foldm
readP     = regexP "read"         $ readm
retrP     = regexP "retr"         $ retrm 
greetingP = regexP "greeting" $ greeting

transition :: Monad m => String -> [(Regexish, m ())] -> m ()
transition s []       = return ()
transition s (x:xs) 
  | matches (fst x) s = snd x
  | otherwise         = transition s xs 

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

parseRead :: String -> Int
parseRead = read . drop 5

size :: String -> CSpecS Int String ()
size trans = do
  rep   <- get cccmP
  let sz = read (tail rep)
  case trans of
    "READ" -> return ()
    xs     -> store (parseRead xs) 

  next <- send $ quitP ||| foldP ||| readP ||| retrP
  transition next [
    (quitm, stop),
    (foldm, nmbr),
    (readm, size next),
    (retrm, xfer sz)]

xfer :: Int -> CSpecS Int String ()
xfer sz = do
  get $ predicate ("hasSize "++(show sz)) (replicateM sz arbitrary, \xs -> length (xs :: String) == sz)
  size "READ" -- Hack for now, will implement ACK messages later

rfc :: CSpecS Int String ()
rfc = call
