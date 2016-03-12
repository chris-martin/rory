module Rory.App (main) where

import qualified Rory.Server
import qualified Rory.Args

main :: IO ()
main = do args <- Rory.Args.get
          Rory.Server.run args
          return ()
