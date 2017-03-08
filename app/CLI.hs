module CLI
       ( Config(..)
       , readConfig
       ) where

import Network.Socket (HostName, PortNumber)
import Data.Text
import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config
              { serverHost :: HostName
              , serverPort :: PortNumber
              , usingSSL :: Bool
              , nick :: Text
              , channel :: Text
              }

configParser :: Parser Config
configParser = Config
  <$> strOption (long "host"
                 <> metavar "HOST"
                 <> help "Server to connect to")
  <*> option auto (long "port"
                   <> metavar "PORT"
                   <> help "Port on the HOST")
  <*> switch (long "ssl")
  <*> (pack <$> strOption (long "nick"))
  <*> (pack <$> strOption (long "channel"))

readConfig :: IO Config
readConfig = execParser opts
  where
    opts = info (configParser <**> helper)
      (fullDesc
       <> progDesc "Print a greeting for TARGET"
       <> header "hello - a test for optparse-applicative")
