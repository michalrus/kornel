{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kornel.Config
  ( Config(..)
  , readConfig
  ) where

import qualified Data.Text           as T
import qualified Dhall
import qualified Kornel.IrcParser    as I
import           Network.Socket      (HostName, PortNumber)
import qualified Options.Applicative as O

data Config = Config
  { serverHost :: HostName
  , serverPort :: PortNumber
  , usingSSL :: Bool
  , nick :: I.Target
  , nickservPassword :: Maybe Text
  , cleverBotApiKey :: Maybe Text
  , haskellBotNicks :: [I.Target]
  , scalaBotNicks :: [I.Target]
  , httpSnippetsFetchMax :: Integer
  , channels :: [I.Target]
  , logTraffic :: Bool
  } deriving (Eq, Generic, Show)

instance Dhall.Interpret Config

instance Dhall.Interpret PortNumber where
  autoWith _ = fromIntegral <$> Dhall.natural

deriving instance Dhall.Interpret I.Target

readConfig :: IO Config
readConfig = do
  cli <- readCLI
  Dhall.input Dhall.auto (dhallExpr cli) <&>
    (\c ->
       c
         { nickservPassword = map T.strip . nickservPassword $ c
         , cleverBotApiKey = map T.strip . cleverBotApiKey $ c
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
