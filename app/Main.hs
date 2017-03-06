{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.IO.Error
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Char8 as B
import Network.Connection
import Network.Socket
import qualified IrcParser

data Config = Config
              { serverHost :: HostName
              , serverPort :: PortNumber
              , nick :: String
              , channel :: String
              }

configParser :: Parser Config
configParser = Config
  <$> strOption (long "host"
                 <> metavar "HOST"
                 <> help "Server to connect to")
  <*> option auto (long "port"
                   <> metavar "PORT"
                   <> help "Port on the HOST")
  <*> strOption (long "nick")
  <*> strOption (long "channel")

main :: IO ()
main = runConfig =<< execParser opts
  where
    opts = info (configParser <**> helper)
      (fullDesc
       <> progDesc "Print a greeting for TARGET"
       <> header "hello - a test for optparse-applicative")

realName :: String
realName = "https://github.com/michalrus/kornel"

runConfig :: Config -> IO ()
runConfig cfg = do
  ctx <- initConnectionContext
  con <- login cfg ctx
  catchIOError (forever $ processLine con)
    (\e -> if isEOFError e then return () else ioError e)

login :: Config -> ConnectionContext -> IO Connection
login cfg ctx = do
  con <- connectTo ctx $ ConnectionParams
                            { connectionHostname  = serverHost cfg
                            , connectionPort      = serverPort cfg
                            , connectionUseSecure = Nothing
                            , connectionUseSocks  = Nothing
                            }
  connectionPut con $ B.pack $
    "NICK " ++ (nick cfg) ++ "\r\n" ++
    "USER " ++ (nick cfg) ++ " - - :" ++ realName ++ "\r\n" ++
    "JOIN " ++ (channel cfg) ++ "\r\n"
  return con

processLine :: Connection -> IO ()
processLine con = do
  raw <- trimCR <$> connectionGetLine 512 con
  case IrcParser.readMessage raw of
    Left  err -> hPutStrLn stderr $ "Failed to parse message ‘" ++ show raw ++ "’ with ‘" ++ show err ++ "’"
    Right ok  -> putStrLn $ show ok
  where
    trimCR ln = if B.last ln == '\r' then B.init ln else ln
