module Rory.Server.Name
    ( middleware
    , serverName
    ) where

import qualified Data.ByteString.Char8 as S8
import Data.List (intercalate)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hServer)
import Network.Wai (Middleware, modifyResponse, mapResponseHeaders)
import Network.Wai.Handler.Warp (warpVersion)
import Rory.Core (roryVersion)

middleware :: Middleware
middleware = modifyResponse (mapResponseHeaders (serverHeader :))

serverHeader :: Header
serverHeader = (hServer, S8.pack serverName)

serverName :: String
serverName = concat
    [ "Rory "
    , roryVersion
    , " (built with Warp "
    , warpVersion
    , ")"
    ]
