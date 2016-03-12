module Rory.Server.Name
    ( middleware
    , serverName
    ) where

import Rory.Version (roryVersion)

import qualified Data.ByteString.Char8 as S8

import Network.HTTP.Types        (Header)
import Network.HTTP.Types.Header (hServer)
import Network.Wai               (Middleware, mapResponseHeaders,
                                  modifyResponse)
import Network.Wai.Handler.Warp  (warpVersion)

middleware :: Middleware
middleware = modifyResponse $ mapResponseHeaders (serverHeader :)

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
