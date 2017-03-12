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
              , nickservPasswordFile :: Maybe FilePath
              , cleverBotApiKeyFile :: Maybe FilePath
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
  <*> (optional $ strOption (long "nickserv-password-file"))
  <*> (optional $ strOption (long "cleverbot-api-key-file"))
  <*> (pack <$> strOption (long "channel"))

readConfig :: IO Config
readConfig = execParser opts
  where
    opts = info (configParser <**> helper)
      (fullDesc
       <> header "kornel — a simple IRC bot")
