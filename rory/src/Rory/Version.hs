module Rory.Version (roryVersion) where

import Data.Version (showVersion)

import qualified Paths_rory

roryVersion :: String
roryVersion = showVersion Paths_rory.version
