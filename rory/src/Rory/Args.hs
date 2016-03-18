module Rory.Args
     ( Args (..)
     , parser
     , parserInfo
     , get
     , bindHostOrDefault
     , bindPortOrDefault
     ) where

import Data.Maybe               (fromMaybe)
import Data.String              (fromString)
import Network.Wai.Handler.Warp (Port, HostPreference)
import Options.Applicative

get :: IO Args
get = execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo = info (helper <*> parser) fullDesc

data Args = Args
    { configFile :: Maybe FilePath
    , bindPort   :: Maybe Port
    , bindHost   :: Maybe HostPreference
    , pidFile    :: Maybe FilePath
    , dryRun     :: Bool
    , command    :: Maybe Command
    } deriving Show

data Command = Start | Stop | Restart | Reload deriving Show

commandReader :: ReadM Command
commandReader = str >>= \s -> case s of
    "start"   -> pure Start
    "stop"    -> pure Stop
    "restart" -> pure Restart
    "reload"  -> pure Reload
    _         -> readerError $ "Invalid command: '" ++ s ++ "'"

parser :: Parser Args
parser = Args
    <$> (optional $ option str
          ( long "config-file"
         <> short 'c'
         <> metavar "CONFIG_FILE"))
    <*> (optional $ option auto
          ( long "bind-port"
         <> metavar "BIND_PORT"
         <> help ("Default: " ++ show defaultBindPort)))
    <*> (optional $ option (fromString <$> str)
          ( long "bind-host"
         <> metavar "BIND_HOST"))
    <*> (optional $ option str
          ( long "pid-file"
         <> metavar "PID_FILE"))
    <*> (switch $ long "dry-run")
    <*> (optional $ argument commandReader
          (metavar "COMMAND"))

defaultBindPort :: Port
defaultBindPort = 58594

bindPortOrDefault :: Args -> Port
bindPortOrDefault = fromMaybe defaultBindPort . bindPort

defaultHost :: HostPreference
defaultHost = fromString "*"

bindHostOrDefault :: Args -> HostPreference
bindHostOrDefault = fromMaybe defaultHost . bindHost
