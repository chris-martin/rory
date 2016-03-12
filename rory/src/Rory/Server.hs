{-# LANGUAGE OverloadedStrings #-}

module Rory.Server (run) where

import           Rory.Args        (Args)
import           Rory.Config      (Config)
import qualified Rory.Config      as Config
import qualified Rory.Server.Name

import qualified Blaze.ByteString.Builder.ByteString as BBS
import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource        as Res
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Char8               as S8
import           Data.IORef                          (IORef, newIORef,
                                                      readIORef, writeIORef)
import qualified Data.Text                           as Text
import           Network.HTTP.Types                  (status200)
import qualified Network.Wai                         as Wai
import qualified Network.Wai.Handler.Warp            as Warp
import qualified Network.Wai.Parse                   as NWP
import           System.Directory                    (renameFile)
import qualified System.Posix.Signals                as Sig
import qualified Systemd.Journal                     as J

run :: Args -> IO ()
run args = do
    server <- initServer args
    _ <- installHupHandler server
    Warp.run 3000 $ middleware $ application server
  where
    middleware = Rory.Server.Name.middleware

-- | Set up the HUP signal handler to reload the config.
installHupHandler :: Server -> IO ()
installHupHandler server = do
    _ <- Sig.installHandler Sig.sigHUP handler Nothing
    return ()
  where
    handler = Sig.Catch $ do
        J.sendMessage $ Text.pack "Received HUP signal"
        loadConfig server

data Server = Server
    { serverArgs   :: Args
    , serverConfig :: IORef Config
    }

initServer :: Args -> IO Server
initServer args = do
    config <- Config.load args
    configVar <- newIORef config
    return Server { serverArgs   = args
                  , serverConfig = configVar }

loadConfig :: Server -> IO ()
loadConfig server = do
   config <- Config.load $ serverArgs server
   writeIORef (serverConfig server) config

application :: Server -> Wai.Application
application server request respond = do
    _ <- runResourceT $ Res.withInternalState $ \is -> do
        (_params, files) <- NWP.parseRequestBody (tempFileBackEnd is) request
        _ <- renameFile (NWP.fileContent $ snd $ head files) "/tmp/upload"
        return ()
    config <- readIORef $ serverConfig server
    received <- respond $ stringResponse $ show config
    return received

tempFileBackEnd :: Res.InternalState -> ignored1 -> ignored2 -> IO BS.ByteString -> IO FilePath
tempFileBackEnd = NWP.tempFileBackEndOpts (pure "/tmp") "webenc.buf"

stringResponse :: String -> Wai.Response
stringResponse body = Wai.responseBuilder
    status200
    [("Content-Type", "text/plain")]
    (BBS.fromByteString $ S8.pack body)
