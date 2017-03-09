{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import System.IO.Error
import Control.Monad
import Data.Maybe (maybeToList)
import Data.Text as Text -- bug in Intero?
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import qualified Data.Text.IO as TIO
import Network.Connection
import qualified IrcParser as I
import CLI

main :: IO ()
main = do
  mapM_ (flip hSetEncoding utf8) [stdout, stdin, stderr]
  readConfig >>= runConfig

runConfig :: Config -> IO ()
runConfig cfg = do
  ctx <- initConnectionContext
  con <- login cfg ctx
  catchIOError (forever $ processLine cfg con)
    (\e -> if isEOFError e then return () else ioError e)

login :: CLI.Config -> ConnectionContext -> IO Connection
login cfg ctx = do
  con <- connectTo ctx $ ConnectionParams
                            { connectionHostname  = serverHost cfg
                            , connectionPort      = serverPort cfg
                            , connectionUseSecure =
                              if (not $ usingSSL cfg) then Nothing
                              else Just $ TLSSettingsSimple
                                   { settingDisableCertificateValidation = False
                                   , settingDisableSession = True
                                   , settingUseServerName = True
                                   }
                            , connectionUseSocks  = Nothing
                            }
  nickservCmd <- flip traverse (nickservPasswordFile cfg) $
    \path -> do
      pw <- strip <$> TIO.readFile path
      return $ I.Privmsg "NickServ" (append "IDENTIFY " $ pw)
  mapM_ (sendCommand con) $
    [ I.Nick $ nick cfg
    , I.User (nick cfg) "-" "-" "https://github.com/michalrus/kornel"
    , I.Join [channel cfg]
    ] ++ maybeToList nickservCmd
  return con

processLine :: Config -> Connection -> IO ()
processLine cfg con = do
  raw <- dropWhileEnd isEndOfLine <$> (decodeUtf8With $ \_ _ -> Just '_') <$> connectionGetLine 512 con
  case I.readMessage raw of
    Left  err -> hPutStrLn stderr $ "Failed to parse message ‘" ++ show raw ++ "’ with ‘" ++ show err ++ "’"
    Right ok  -> do
      response <- processMsg cfg ok
      case response of
        Nothing -> return ()
        Just r -> sendCommand con r
  where
    isEndOfLine c = c == '\r' || c == '\n'

sendCommand :: Connection -> I.IrcCommand -> IO ()
sendCommand con cmd = do
  putStrLn $ "-> " ++ show cmd
  connectionPut con $ encodeUtf8 $ append (I.showCommand cmd) "\r\n"

processMsg :: Config -> I.IrcLine -> IO (Maybe I.IrcCommand)
processMsg cfg (I.IrcLine origin msg) = do
  putStrLn $ "<- " ++ show origin ++ " - " ++ show msg
  case (origin, msg) of
    (_,         I.Ping t)      -> return $ Just $ I.Pong t
    (Just mask, I.Privmsg t m) -> processPrivmsg cfg mask t m
    _ -> return Nothing

processPrivmsg :: Config -> I.Hostmask -> Text -> Text -> IO (Maybe I.IrcCommand)
processPrivmsg cfg mask target message =
  if (toUpper $ nick cfg) `isInfixOf` (toUpper $ message) then
    return $ Just $ I.Privmsg replyTo $ Text.concat [I.nick mask, ": co tam? Zażółć gęślą jaźń! 😼"]
  else return Nothing
  where
    replyTo = if (target /= nick cfg) then target else I.nick mask
