module Rory.App (main) where

import qualified Rory.Args   as Args
import qualified Rory.DryRun as DryRun
import qualified Rory.Server as Server

main :: IO ()
main = do args <- Args.get
          if Args.dryRun args
          then DryRun.dryRun args
          else Server.run args
