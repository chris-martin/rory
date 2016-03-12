module Rory.Version
    ( roryVersion
    , serverName
    ) where

import Data.Version (showVersion)
import Network.Wai.Handler.Warp (warpVersion)

import qualified Paths_rory

roryVersion :: String
roryVersion = showVersion Paths_rory.version

serverName :: String
serverName = concat
    [ "Rory "
    , roryVersion
    , " (built with Warp "
    , warpVersion
    , ")"
    ]
