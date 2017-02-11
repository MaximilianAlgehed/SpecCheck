{-# LANGUAGE TypeApplications #-}
import ST
import CSpec
import Predicate

incoherent :: Spec Int Int
incoherent = do
  n1 <- send (posNum @Int)
  n2 <- send negNum
  get $ inRange (n1, n2)

main = coherent incoherent 
