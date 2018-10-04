

import           Debug
import           GHC.IO.Encoding

main :: IO ()
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  p3
  q
  w
