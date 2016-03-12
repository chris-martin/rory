module Rory.Server.Args
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
    }

parser :: Parser Args
parser = Args
    <$> (optional $ strOption
          ( long "config-file"
         <> short 'c'
         <> metavar "CONFIG_FILE" ))
