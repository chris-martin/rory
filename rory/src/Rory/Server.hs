module Rory.Server (run) where

import           Rory.Args    (Args)
import qualified Rory.Args    as Args
import           Rory.Config  (Config)
import qualified Rory.Config  as Config
import qualified Rory.Version

import qualified Blaze.ByteString.Builder.ByteString as BBS

import           Control.Monad.Trans.Resource
import qualified Control.Monad.Trans.Resource as Res

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as S8
import           Data.IORef            (IORef, newIORef, readIORef, writeIORef)
import           Data.List             (intercalate)
import qualified Data.Text             as Text

import           Network.HTTP.Types       (hContentType, status200, status405)
import qualified Network.HTTP.Types       as H
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Parse        as NWP

import           System.Directory     (renameFile)
import qualified System.Posix.Signals as Sig

import qualified Systemd.Journal as J

run :: Args -> IO ()
run args = do
    J.sendMessage $ Text.pack $ "Args: " ++ show args
    server <- initServer args
    _ <- installHupHandler server
    J.sendMessage $ Text.pack "Warp: starting"
    Warp.runSettings (warpSettings args) (application server)
    J.sendMessage $ Text.pack "Warp: stopped"

warpSettings :: Args -> Warp.Settings
warpSettings args =
    (Warp.setHost $ Args.bindHostOrDefault args) $
    (Warp.setPort $ Args.bindPortOrDefault args) $
    (Warp.setServerName $ S8.pack Rory.Version.serverName) $
    (Warp.setLogger warpLogger) $
    (Warp.setInstallShutdownHandler installIntHandler)
    Warp.defaultSettings

warpLogger :: Wai.Request -> H.Status -> Maybe Integer -> IO ()
warpLogger req status _size = J.sendMessage $ Text.pack $ intercalate " "
    [ S8.unpack $ Wai.requestMethod req
    , S8.unpack $ Wai.rawPathInfo req
    , show $ H.statusCode status ]

-- | Set up the HUP signal handler to reload the config.
installHupHandler :: Server -> IO ()
installHupHandler server = const () <$> install
  where install = Sig.installHandler Sig.sigHUP handler Nothing
        handler = Sig.Catch $ do
            J.sendMessage $ Text.pack "Received HUP signal"
            loadConfig server

-- | Set up the INT signal handler to stop the server.
installIntHandler :: IO () -> IO ()
installIntHandler stop = const () <$> install
  where install = Sig.installHandler Sig.sigINT handler Nothing
        handler = Sig.Catch $ do
            J.sendMessage $ Text.pack $ "Warp: stopping..."
            stop

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
    case H.parseMethod $ Wai.requestMethod request of
        Right(H.GET) -> respond $ plain status200 $ S8.pack "Hi.\n"
        Right(H.PUT) -> put server request respond
        _ -> respond $ plain status405 $ S8.pack "Only GET and PUT are supported."

put :: Server -> Wai.Application
put server request respond = do
    _ <- runResourceT $ Res.withInternalState $ \is -> do
        (_params, files) <- NWP.parseRequestBody (tempFileBackEnd is) request
        _ <- renameFile (NWP.fileContent $ snd $ head files) "/tmp/upload"
        return ()
    config <- readIORef $ serverConfig server
    received <- respond $ plain status200 $ S8.pack $ (show config ++ "\n")
    return received

tempFileBackEnd :: Res.InternalState -> ignored1 -> ignored2 -> IO BS.ByteString -> IO FilePath
tempFileBackEnd = NWP.tempFileBackEndOpts (pure "/tmp") "webenc.buf"

plain :: H.Status -> ByteString -> Wai.Response
plain status body = Wai.responseBuilder
    status
    [(hContentType, S8.pack "text/plain")]
    (BBS.fromByteString $ body)
