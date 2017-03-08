{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import System.IO.Error
import Control.Monad
import Data.Text
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Network.Connection
import qualified IrcParser as I
import CLI

main :: IO ()
main = readConfig >>= runConfig

runConfig :: Config -> IO ()
runConfig cfg = do
  ctx <- initConnectionContext
  con <- login cfg ctx
  catchIOError (forever $ processLine con)
    (\e -> if isEOFError e then return () else ioError e)

login :: CLI.Config -> ConnectionContext -> IO Connection
login cfg ctx = do
  con <- connectTo ctx $ ConnectionParams
                            { connectionHostname  = serverHost cfg
                            , connectionPort      = serverPort cfg
                            , connectionUseSecure = Nothing
                            , connectionUseSocks  = Nothing
                            }
  mapM_ (sendCommand con)
    [ I.NickCommand $ nick cfg
    , I.UserCommand (nick cfg) "-" "-" "https://github.com/michalrus/kornel"
    , I.JoinCommand [channel cfg]
    ]
  return con

processLine :: Connection -> IO ()
processLine con = do
  raw <- dropWhileEnd isCRLF <$> (decodeUtf8With $ \_ _ -> Just '_') <$> connectionGetLine 512 con
  case I.readMessage raw of
    Left  err -> hPutStrLn stderr $ "Failed to parse message ‘" ++ show raw ++ "’ with ‘" ++ show err ++ "’"
    Right ok  -> do
      response <- processMsg ok
      case response of
        Nothing -> return ()
        Just r -> sendCommand con r
  where
    isCRLF c = c == '\r' || c == '\n'

sendCommand :: Connection -> I.IrcCommand -> IO ()
sendCommand con cmd = do
  putStrLn $ "-> " ++ show cmd
  connectionPut con $ encodeUtf8 $ append (I.showCommand cmd) "\r\n"

processMsg :: I.IrcMessage -> IO (Maybe I.IrcCommand)
processMsg (I.IrcMessage origin msg) = do
  putStrLn $ "<- " ++ show origin ++ " - " ++ show msg
  case msg of
    I.PingCommand t -> return . Just $ I.PongCommand t
    I.PrivmsgCommand ch m ->
      if isPrefixOf "kornel" m then
         return . Just $ I.PrivmsgCommand ch "co tam? Zażółć gęślą jaźń! 😼"
      else return Nothing
    _ -> return Nothing
