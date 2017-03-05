{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO.Error
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Char8 as B
import Network.Connection
import Network.Socket

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
runConfig cfg =
  do
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
  r <- connectionGetLine 512 con
  putStrLn $ show r
