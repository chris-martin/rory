{-# LANGUAGE OverloadedStrings #-}

module Rory.Server
    ( main
    ) where

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Parse as NWP
import Network.HTTP.Types (status200)
import System.Directory (renameFile)
import qualified Control.Monad.Trans.Resource as Res
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS
import qualified Blaze.ByteString.Builder.ByteString as BBS

import qualified Rory.Server.Name

application :: Wai.Application
application request respond = do
  _ <- runResourceT $ Res.withInternalState $ \is -> do
    (_params, files) <- NWP.parseRequestBody (tempFileBackEnd is) request
    _ <- renameFile (NWP.fileContent $ snd $ head files) "/tmp/upload"
    return ()
  received <- respond (stringResponse Rory.Server.Name.serverName)
  return received

tempFileBackEnd :: Res.InternalState -> ignored1 -> ignored2 -> IO BS.ByteString -> IO FilePath
tempFileBackEnd = NWP.tempFileBackEndOpts (pure "/tmp") "webenc.buf"

stringResponse :: String -> Wai.Response
stringResponse body = Wai.responseBuilder
   status200
   [("Content-Type", "text/plain")]
   (BBS.fromByteString $ S8.pack body)

main :: IO ()
main = run 3000 $ middleware application
  where middleware = Rory.Server.Name.middleware
