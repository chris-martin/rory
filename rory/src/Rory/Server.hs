{-# LANGUAGE OverloadedStrings #-}

module Rory.Server
    ( main
    ) where

import qualified Rory.Server.Args
import qualified Rory.Server.Name

import qualified Blaze.ByteString.Builder.ByteString as BBS
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource        as Res
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Char8               as S8
import           Network.HTTP.Types                  (status200)
import qualified Network.Wai                         as Wai
import           Network.Wai.Handler.Warp            (run)
import qualified Network.Wai.Parse                   as NWP
import           System.Directory                    (renameFile)
import qualified System.Posix.Signals                as Sig

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
main = do args <- Rory.Server.Args.get
          _    <- Sig.installHandler Sig.sigHUP Sig.Ignore Nothing
          _    <- run 3000 $ middleware application
          return ()
  where middleware = Rory.Server.Name.middleware
