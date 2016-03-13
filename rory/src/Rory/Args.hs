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
    } deriving Show

parser :: Parser Args
parser = Args
    <$> (optional $ strOption
          ( long "config-file"
         <> short 'c'
         <> metavar "CONFIG_FILE" ))
    <*> (optional $ strOption
          ( long "pid-file"
         <> metavar "PID_FILE" ))
