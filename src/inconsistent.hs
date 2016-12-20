import ST
import CSpec
import Predicate

inconsistent :: CSpec Int ()
inconsistent = do
  n1 <- (send posNum :: CSpec Int Int)
  n2 <- send negNum
  get (inRange (n1, n2))
  stop
