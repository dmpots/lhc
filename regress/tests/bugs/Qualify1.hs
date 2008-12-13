module Main where

import qualified Prelude as P

data T = T

-- GHC doesn't allow: T.show T = "T". What does the haskell98 doc say?
instance P.Show T where
  show T = "T"

main :: P.IO ()
main = P.return ()

