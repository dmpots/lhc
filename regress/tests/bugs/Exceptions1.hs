module Main where

import Control.Exception
import Prelude hiding (catch)

main :: IO ()
main = (error "Catch me!" `seq` return ()) `catch` \_ -> return ()
