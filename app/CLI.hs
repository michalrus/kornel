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
              , httpSnippetsFetchMax :: Int
              , channel :: Text
              , verbose :: Bool
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
  <*> option auto (long "http-snippets-fetch-max"
                   <> metavar "BYTES"
                   <> help "This many bytes will be read from each document, until a <title/> is found.")
  <*> (pack <$> strOption (long "channel"))
  <*> switch (long "verbose")

readConfig :: IO Config
readConfig = execParser opts
  where
    opts = info (configParser <**> helper)
      (fullDesc
       <> header "kornel — a simple IRC bot")
