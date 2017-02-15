> {-# LANGUAGE OverloadedStrings #-}
> import Typeclasses
> import ST
> import Predicate
> import CSpec
> import Regexish
> import Test.QuickCheck
> import Control.Monad

We specify the possible messages as regular-ish expressions
 
> helom    = "HELO " >*> word >*> " " >*> word 
> nnnm     = "#"     >*> integer
> cccm     = "="     >*> integer
> quitm    = "QUIT"
> foldm    = "FOLD " >*> word
> readm    = "READ"  >*> possibly (" " >*> integer)
> retrm    = "RETR"
> greeting = "POP2 " >*> Anything
> ackdm    = "ACKD"
> acksm    = "ACKS"

|regexP| allows us to turn a |Regexish| in to a |Predicate|
 
> heloP     = regexP "HELO_message" helom 
> nnnmP     = regexP "nnn"          nnnm
> cccmP     = regexP "ccc"          cccm
> quitP     = regexP "quit"         quitm
> foldP     = regexP "fold"         foldm
> readP     = regexP "read"         readm
> retrP     = regexP "retr"         retrm 
> greetingP = regexP "greeting"     greeting
> ackdP     = regexP "ackd"         ackdm
> acksP     = regexP "acks"         acksm 

There are four "states" the protocol can be in
and it always starts out in the call state, where
the sever sends a |greeting| message and the client may
either |quit| or send a |helo| message.
 
> call :: SpecS Int String ()
> call = do
>   get greetingP
> 
>   next <- send $ quitP ||| heloP
>   transition next [
>     (quitm,  stop),
>     (helom,  nmbr)]

The |nmbr| state is entered every time a new folder is accessed,
either from the client sending a |helo| message (i.e. on login)
or by using the |fold| command.
 
> nmbr :: SpecS Int String ()
> nmbr = do
>   store 1
>
>   numMsgs <- get nnnmP
> 
>   next    <- send $ quitP ||| foldP ||| readP
>   transition next [
>     (quitm, stop),
>     (readm, size next),
>     (foldm, nmbr)]
 
> size :: String -> SpecS Int String ()
> size trans = do
>   case trans of
>     "READ" -> return ()
>     "ACKD" -> modify (+1)
>     "ACKS" -> modify (+1)
>     xs     -> store (read $ drop 5 xs) 
> 
>   rep   <- get cccmP
>   let sz = read (tail rep)
> 
>   next <- send $ quitP ||| foldP ||| readP ||| retrP
>   transition next [
>     (quitm, stop),
>     (foldm, nmbr),
>     (readm, size next),
>     (retrm, xfer sz)]
 
> xfer :: Int -> SpecS Int String ()
> xfer sz = do
>   get $ predicate ("hasSize "++(show sz)) (replicateM sz arbitrary, \xs -> length (xs :: String) == sz)
>   ack <- send $ ackdP ||| acksP
>   size ack
 
> rfc :: SpecS Int String ()
> rfc = call
