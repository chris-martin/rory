module Rory.Config (load, Config(..)) where

import           Rory.Args (Args)
import qualified Rory.Args as Args

import qualified Data.ByteString as BS

data Config = Config
    { fileContent :: Maybe BS.ByteString
    } deriving Show

empty :: Config
empty = Config { fileContent = Nothing }

fromFileContent :: BS.ByteString -> Config
fromFileContent x = Config { fileContent = Just x }

load :: Args -> IO Config
load args = maybe (return empty) loadFile $ Args.configFile args

loadFile :: FilePath -> IO Config
loadFile path = fromFileContent <$> BS.readFile path
