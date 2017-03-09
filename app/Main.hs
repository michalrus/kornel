{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.IO
import System.IO.Error
import System.Timeout
import Control.Exception.Base (catch, SomeException)
import Control.Monad
import Control.Monad.Loops (whileJust_, iterateWhile)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.Chan
import Data.Maybe (maybeToList)
import Data.Text as Text -- bug in Intero?
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
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
  forever $ do
    hPutStrLn stderr "Starting new session…"
    catch (runSession cfg ctx) $ \(e :: SomeException) ->
      hPutStrLn stderr $ "Error: " ++ show e
    hPutStrLn stderr "Session ended."
    threadDelay $ 10 * 1000 * 1000 -- µs

data IpcQueue
  = QServerLine I.IrcLine
  | QQuit
  | QClientLine I.IrcCommand

runSession :: Config -> ConnectionContext -> IO ()
runSession cfg ctx = do
  con <- login cfg ctx
  ipc <- newChan
  let pingEvery = 20 * 1000 * 1000 -- µs
  -- parse & enqueue lines from server
  _ <- forkIO $ do
    let timedProcessLine = join <$> (timeout (2 * pingEvery) $ processRawLine con)
    whileJust_ timedProcessLine $ writeChan ipc . QServerLine
    writeChan ipc QQuit
  -- send our PINGs
  thrPing <- forkIO $ forever $ do
    threadDelay pingEvery
    uuid <- UUID.nextRandom
    writeChan ipc $ QClientLine $ I.Ping $ pack $ UUID.toString uuid
  -- process the queue
  _ <- iterateWhile id $ do
    readChan ipc >>= \case
      QQuit -> return False
      QClientLine cmd -> sendCommand con cmd >> return True
      QServerLine ln -> (forkIO $ handleLine cfg ln >>= mapM_ (writeChan ipc . QClientLine)) >> return True
  killThread thrPing
  return ()

login :: Config -> ConnectionContext -> IO Connection
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

processRawLine :: Connection -> IO (Maybe I.IrcLine)
processRawLine con =
  flip catchIOError (\e -> if isEOFError e then return Nothing else ioError e) $ do
    rawBytes <- connectionGetLine 1024 con
    raw <- dropWhileEnd isEndOfLine <$> (decodeUtf8With $ \_ _ -> Just '_') <$> pure rawBytes
    case I.readMessage raw of
      Left  err -> do
        hPutStrLn stderr $ "Failed to parse message ‘" ++ show raw ++ "’ with ‘" ++ show err ++ "’"
        return Nothing
      Right ok  -> return $ Just ok
    where
      isEndOfLine c = c == '\r' || c == '\n'

sendCommand :: Connection -> I.IrcCommand -> IO ()
sendCommand con cmd = do
  putStrLn $ "-> " ++ show cmd
  connectionPut con $ encodeUtf8 $ append (I.showCommand cmd) "\r\n"

handleLine :: Config -> I.IrcLine -> IO (Maybe I.IrcCommand)
handleLine cfg (I.IrcLine origin msg) = do
  putStrLn $ "<- " ++ show origin ++ " - " ++ show msg
  case (origin, msg) of
    (_,         I.Ping t)      -> return $ Just $ I.Pong t
    (Just mask, I.Privmsg t m) -> handlePrivmsg cfg mask t m
    _ -> return Nothing

handlePrivmsg :: Config -> I.Hostmask -> Text -> Text -> IO (Maybe I.IrcCommand)
handlePrivmsg cfg mask target message =
  if (toUpper $ nick cfg) `isInfixOf` (toUpper $ message) then
    return $ Just $ I.Privmsg replyTo $ Text.concat [I.nick mask, ": co tam? Zażółć gęślą jaźń! 😼"]
  else return Nothing
  where
    replyTo = if (target /= nick cfg) then target else I.nick mask
