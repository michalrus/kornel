module Kornel.CLI
       ( Config(..)
       , readConfig
       , LogLevel(..)
       ) where

import Network.Socket (HostName, PortNumber)
import Data.Text
import Options.Applicative
import Data.Semigroup ((<>))
import qualified IrcParser as I

data LogLevel = LogInfo | LogDebug deriving (Show, Eq)

data Config = Config
              { serverHost :: HostName
              , serverPort :: PortNumber
              , usingSSL :: Bool
              , nick :: I.Target
              , nickservPasswordFile :: Maybe FilePath
              , cleverBotApiKeyFile :: Maybe FilePath
              , haskellBotNicks :: [I.Target]
              , scalaBotNicks :: [I.Target]
              , httpSnippetsFetchMax :: Int
              , channels :: [I.Target]
              , verbose :: LogLevel
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
  <*> option target (long "nick")
  <*> optional (strOption (long "nickserv-password-file"))
  <*> optional (strOption (long "cleverbot-api-key-file"))
  <*> option targets (long "haskell-bot-nicks" <> metavar "NICK1[,…]")
  <*> option targets (long "scala-bot-nicks" <> metavar "NICK1[,…]")
  <*> option auto (long "http-snippets-fetch-max"
                   <> metavar "BYTES"
                   <> help "This many bytes will be read from each document, until a <title/> is found.")
  <*> option targets (long "channels" <> metavar "CHANNEL1[,…]")
  <*> flag LogInfo LogDebug (long "verbose")
  where
    text = pack <$> str
    texts = splitOn "," <$> text
    target = I.Target <$> text
    targets = fmap I.Target <$> texts

readConfig :: IO Config
readConfig = execParser opts
  where
    opts = info (configParser <**> helper)
      (fullDesc
       <> header "kornel — a simple IRC bot")
