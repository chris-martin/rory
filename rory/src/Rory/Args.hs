module Rory.Args
     ( Args (..)
     , parser
     , parserInfo
     , get
     ) where

import Options.Applicative

get :: IO Args
get = execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo = info (helper <*> parser) fullDesc

data Args = Args
    { configFile :: Maybe FilePath
    , bindPort   :: Maybe Int
    , bindHost   :: Maybe String
    , pidFile    :: Maybe FilePath
    , dryRun     :: Bool
    , command    :: Maybe Command
    } deriving Show

data Command = Start | Stop | Restart | Reload deriving Show

commandOption :: ReadM Command
commandOption = str >>= \s -> case s of
    "start"   -> pure Start
    "stop"    -> pure Stop
    "restart" -> pure Restart
    "reload"  -> pure Reload
    _         -> readerError $ "Invalid command: '" ++ s ++ "'"

parser :: Parser Args
parser = Args
    <$> (optional $ strOption
          ( long "config-file"
         <> short 'c'
         <> metavar "CONFIG_FILE"))
    <*> (optional $ option auto
          ( long "bind-port"
         <> metavar "BIND_PORT"))
    <*> (optional $ strOption
          ( long "bind-host"
         <> metavar "BIND_HOST"))
    <*> (optional $ strOption
          ( long "pid-file"
         <> metavar "PID_FILE"))
    <*> (switch $ long "dry-run")
    <*> (optional $ argument commandOption
          (metavar "COMMAND"))
