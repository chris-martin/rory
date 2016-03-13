module Rory.Config (load, Config(..)) where

import           Rory.Args (Args)
import qualified Rory.Args as Args

import qualified Data.ByteString as BS
import qualified Data.Text       as Text
import qualified Systemd.Journal as J

data Config = Config
    { fileContent  :: Maybe BS.ByteString
    , maxFileBytes :: Maybe Int
    } deriving Show

defaultMaxFileBytes = Just $ 10^6

empty :: Config
empty = Config
    { fileContent = Nothing
    , maxFileBytes = defaultMaxFileBytes }

fromFileContent :: BS.ByteString -> Config
fromFileContent x = empty { fileContent = Just x }

load :: Args -> IO Config
load args = maybe (return empty) loadFile $ Args.configFile args

loadFile :: FilePath -> IO Config
loadFile path = do
    J.sendMessage $ Text.pack $ "Reading config from " ++ path
    content <- BS.readFile path
    return $ fromFileContent content
