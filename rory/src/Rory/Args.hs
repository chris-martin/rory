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
    , pidFile    :: Maybe FilePath
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
    <*> (optional $ strOption
          ( long "pid-file"
         <> metavar "PID_FILE"))
    <*> (optional $ argument commandOption
          (metavar "COMMAND"))
