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
helom = Match "HELO " >*> word >*> Match " " >*> word 
nnnm  = Match "#" >*> integer
cccm  = Match "=" >*> integer
quitm = Match "QUIT"
foldm = Match "FOLD " >*> word
readm = Match "READ " >*> integer
retrm = Match "RETR " >*> integer
greeting = Match "POP2 " >*> Anything

heloP = regexP "HELO_message" $ helom 
nnnmP = regexP "Integer"      $ nnnm
cccmP = regexP "Integer"      $ cccm
quitP = regexP "quit"         $ quitm
foldP = regexP "fold"         $ foldm
readP = regexP "read"         $ readm
retrP = regexP "retr"         $ retrm 
greetingP = regexP "greeting" $ greeting

transition :: Monad m => String -> [(Regexish, m ())] -> m ()
transition s []       = return ()
transition s (x:xs) 
  | matches (fst x) s = snd x >> return ()
  | otherwise         = transition s xs 

call :: CSpec String ()
call = do
  get greetingP

  next <- send $ quitP ||| heloP
  transition next [
    (quitm,  stop),
    (helom,  nmbr)]

nmbr :: CSpec String ()
nmbr = do
  numMsgs <- get nnnmP

  next    <- send $ quitP ||| foldP ||| readP
  transition next [
    (quitm, stop),
    (readm, size),
    (foldm, nmbr)]

size :: CSpec String ()
size = do
  rep  <- get cccmP
  let sz = read (tail rep)

  next <- send $ quitP ||| foldP ||| readP ||| retrP
  transition next [
    (quitm, stop),
    (foldm, nmbr),
    (retrm, xfer sz),
    (readm, size)]

xfer :: Int -> CSpec String ()
xfer sz = do
  get $ predicate ("hasSize "++(show sz)) (replicateM sz arbitrary, \xs -> length (xs :: String) == sz)
  size

rfc :: CSpec String ()
rfc = call
