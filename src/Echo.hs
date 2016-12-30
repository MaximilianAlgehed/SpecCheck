import CSpec
import ST
import System.Process
import Predicate
import qualified Network.Simple.TCP as N
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import GHC.IO.Handle

echo :: CSpec String String
echo = do
  s <- send wildcard
  get (is s)

main = do
  runShellTCP "python Echo.py" echo
