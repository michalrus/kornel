{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kornel.Config
  ( Config(..)
  , readConfig
  ) where

import qualified Data.Text           as T
import qualified Dhall
import qualified Irc.Identifier      as I
import           Network.Socket      (HostName, PortNumber)
import qualified Options.Applicative as O

data Config = Config
  { serverHost :: HostName
  , serverPort :: PortNumber
  , usingSSL :: Bool
  , nick :: I.Identifier
  , saslPassword :: Maybe Text
  , nickservPassword :: Maybe Text
  , cleverBotApiKey :: Maybe Text
  , smmryApiKey :: Maybe Text
  , wolframApiKey :: Maybe Text
  , haskellBotNicks :: [I.Identifier]
  , scalaBotNicks :: [I.Identifier]
  , httpSnippetsFetchMax :: Integer
  , channels :: [I.Identifier]
  , logTraffic :: Bool
  } deriving (Eq, Generic, Show)

instance Dhall.Interpret Config

instance Dhall.Interpret PortNumber where
  autoWith _ = fromIntegral <$> Dhall.natural

instance Dhall.Interpret I.Identifier where
  autoWith o = I.mkId <$> Dhall.autoWith o

readConfig :: IO Config
readConfig = do
  cli <- readCLI
  Dhall.input Dhall.auto (dhallExpr cli) <&>
    (\c ->
       c
         { saslPassword = map T.strip . saslPassword $ c
         , nickservPassword = map T.strip . nickservPassword $ c
         , cleverBotApiKey = map T.strip . cleverBotApiKey $ c
         , smmryApiKey = map T.strip . smmryApiKey $ c
         , wolframApiKey = map T.strip . wolframApiKey $ c
         })

newtype CLI = CLI
  { dhallExpr :: Text
  } deriving (Show, Eq)

readCLI :: IO CLI
readCLI = O.execParser opts
  where
    opts =
      O.info
        (cliParser <**> O.helper)
        (O.fullDesc <> O.header "kornel — a simple IRC bot")
    cliParser :: O.Parser CLI
    cliParser =
      CLI <$>
      (pack <$>
       O.strOption
         (O.long "config-expr" <> O.short 'c' <> O.metavar "EXPR" <>
          O.help "Configuration expression to use."))
