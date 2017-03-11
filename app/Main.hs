module Main where

import System.IO
import System.IO.Error
import System.Timeout
import Control.Exception.Base (catch, SomeException)
import Control.Monad
import Control.Monad.Loops (whileJust_)
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
import LineHandler
import qualified LineHandler.Chatter
import qualified LineHandler.Slap

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
  = QQuit
  | QClientLine I.IrcCommand
  | QServerLine I.IrcLine
  | QUpdateHandler Int LineHandler

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
  -- process the queue, running each handler for each message on its own thread
  let keepProcessingWith handlers = do
        readChan ipc >>= \case
          QQuit -> return ()
          QUpdateHandler handlerId new ->
            let editNth n x xs = if (0 <= n && n < Prelude.length xs) then
                                   Prelude.take n xs ++ [x] ++ Prelude.drop (n + 1) xs
                                 else xs
            in keepProcessingWith $ editNth handlerId new handlers
          QClientLine cmd -> sendCommand con cmd >> keepProcessingWith handlers
          QServerLine ln -> do
            forM_ ([0..] `Prelude.zip` handlers) $ \(handlerId, Handler handler) -> do
              _ <- forkIO $ do
                (resp, newHandler) <- handler cfg ln
                writeChan ipc $ QUpdateHandler handlerId newHandler
                mapM_ (writeChan ipc . QClientLine) resp
              return ()
            keepProcessingWith handlers
  keepProcessingWith [ handleLogging
                     , handlePing
                     , LineHandler.Chatter.handle
                     , LineHandler.Slap.handle
                     ]
  connectionClose con
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

handlePing :: LineHandler
handlePing = Handler $ \_ -> \case
  I.IrcLine _ (I.Ping t) -> return (Just $ I.Pong t, handlePing)
  _                      -> return (Nothing,         handlePing)

handleLogging :: LineHandler
handleLogging = Handler $ \_ -> \case
  I.IrcLine origin msg -> do
    putStrLn $ "<- " ++ show origin ++ " - " ++ show msg
    return (Nothing, handleLogging)
