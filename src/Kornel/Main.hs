module Kornel.Main
  ( main
  ) where

import qualified Control.Concurrent              as Unsafe
import           Control.Monad.Loops             (whileJust_)
import qualified Data.ByteString                 as BS
import           Data.Coerce
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.UUID                       as UUID
import qualified Data.UUID.V4                    as UUID
import qualified IrcParser                       as I
import           Kornel.CLI
import           Kornel.LineHandler
import qualified Kornel.LineHandler.Chatter
import qualified Kornel.LineHandler.Clojure
import qualified Kornel.LineHandler.Google
import qualified Kornel.LineHandler.Haskell
import qualified Kornel.LineHandler.HttpSnippets
import qualified Kornel.LineHandler.Scala
import qualified Kornel.LineHandler.Slap
import           Network.Connection
import           Prelude                         hiding (Handler)
import qualified System.IO                       as IO
import           System.Timeout

main :: IO ()
main = do
  mapM_ @[_] (`IO.hSetEncoding` IO.utf8) [stdout, stdin, stderr]
  readConfig >>= runConfig

runConfig :: Config -> IO ()
runConfig cfg = do
  ctx <- initConnectionContext
  forever $ do
    IO.hPutStrLn stderr "Starting new session…"
    void $ discardException (runSession cfg ctx)
    IO.hPutStrLn stderr "Session ended."
    threadDelay $ 10 * 1000 * 1000 -- µs

data IpcQueue
  = QQuit
  | QClientLine I.IrcCommand
  | QServerLine I.IrcLine
  | QUpdateHandler Int
                   LineHandler

allHandlers :: [LineHandler]
allHandlers =
  [ handleLogging
  , handlePing
  , Kornel.LineHandler.Chatter.handle
  , Kornel.LineHandler.Slap.handle
  , Kornel.LineHandler.Google.handle
  , Kornel.LineHandler.Clojure.handle
  , Kornel.LineHandler.HttpSnippets.handle
  , Kornel.LineHandler.Scala.handle
  , Kornel.LineHandler.Haskell.handle
  ]

runSession :: Config -> ConnectionContext -> IO ()
runSession cfg ctx = do
  con <- login cfg ctx
  ipc <- newChan
  let pingEvery = 20 * 1000 * 1000 -- µs
  -- parse & enqueue lines from server
  _ <-
    Unsafe.forkIO $ do
      let timedProcessLine =
            join <$> timeout (2 * pingEvery) (processRawLine con)
      whileJust_ timedProcessLine $ writeChan ipc . QServerLine
      writeChan ipc QQuit
  -- send our PINGs
  thrPing <-
    Unsafe.forkIO . forever $ do
      threadDelay pingEvery
      uuid <- UUID.nextRandom
      writeChan ipc . QClientLine . I.Ping . pack $ UUID.toString uuid
  -- process the queue
  void . discardException . iterateWhileJust (pure allHandlers) $
    processQueue cfg con (readChan ipc) (writeChan ipc)
  connectionClose con
  killThread thrPing
  return ()

editNth :: [a] -> Int -> a -> [a]
editNth xs n x =
  if 0 <= n && n < Prelude.length xs
    then Prelude.take n xs ++ [x] ++ Prelude.drop (n + 1) xs
    else xs

iterateWhileJust :: Monad m => m a -> (a -> m (Maybe a)) -> m ()
iterateWhileJust action run = action >>= go
  where
    go an =
      run an >>= \case
        Just an1 -> go an1
        Nothing -> return ()

processQueue ::
     Config
  -> Connection
  -> IO IpcQueue
  -> (IpcQueue -> IO ())
  -> [LineHandler]
  -> IO (Maybe [LineHandler])
processQueue cfg con dequeue enqueue handlers =
  dequeue >>= \case
    QQuit -> return Nothing
    QUpdateHandler handlerId new ->
      return . Just $ editNth handlers handlerId new
    QClientLine cmd -> do
      sendCommand (verbose cfg) con cmd
      return . Just $ handlers
    QServerLine ln -> do
      forM_ ([0 ..] `Prelude.zip` handlers) $ \(handlerId, Handler handler)
        -- running each handler for each message on its own thread
       -> do
        _ <-
          Unsafe.forkIO $ do
            (resp, newHandler) <- handler cfg ln
            enqueue $ QUpdateHandler handlerId newHandler
            mapM_ (enqueue . QClientLine) resp
        return ()
      return . Just $ handlers

login :: Config -> ConnectionContext -> IO Connection
login cfg ctx = do
  con <-
    connectTo
      ctx
      ConnectionParams
        { connectionHostname = serverHost cfg
        , connectionPort = serverPort cfg
        , connectionUseSecure =
            if not $ usingSSL cfg
              then Nothing
              else Just
                     TLSSettingsSimple
                       { settingDisableCertificateValidation = False
                       , settingDisableSession = True
                       , settingUseServerName = True
                       }
        , connectionUseSocks = Nothing
        }
  nickservCmd <-
    for
      (nickservPasswordFile cfg)
      (map (I.Privmsg (I.Target "NickServ") . T.append "IDENTIFY " . T.strip) .
       T.readFile)
  mapM_ (sendCommand (verbose cfg) con) $
    [ I.Nick $ nick cfg
    , I.User
        (I.Username (coerce $ nick cfg))
        (I.Realname "https://github.com/michalrus/kornel")
    , I.Join $ channels cfg
    ] ++
    toList nickservCmd
  return con

processRawLine :: Connection -> IO (Maybe I.IrcLine)
processRawLine con =
  flip
    catchIOError
    (\e ->
       if isEOFError e
         then return Nothing
         else ioError e) $ do
    rawBytes <- connectionGetLine 1024 con
    raw <- T.dropWhileEnd isEndOfLine . decodeUtf8_ <$> pure rawBytes
    case I.readMessage raw of
      Left err -> do
        IO.hPutStrLn stderr $
          "Failed to parse message ‘" ++
          show raw ++ "’ with ‘" ++ show err ++ "’"
        return Nothing
      Right ok -> return $ Just ok
  where
    isEndOfLine c = c == '\r' || c == '\n'

sendCommand :: LogLevel -> Connection -> I.IrcCommand -> IO ()
sendCommand verbosely con cmd = do
  when (verbosely == LogDebug) . putStrLn $ "-> " ++ tshow cmd
  connectionPut con $ BS.append bytes "\r\n"
  where
    bytes = BS.take 510 . encodeUtf8 $ I.showCommand cmd

handlePing :: LineHandler
handlePing =
  Handler $ \_ ->
    \case
      I.IrcLine _ (I.Ping t) -> return (Just $ I.Pong t, handlePing)
      _ -> return (Nothing, handlePing)

handleLogging :: LineHandler
handleLogging =
  Handler $ \cfg ->
    \case
      I.IrcLine origin msg -> do
        when (verbose cfg == LogDebug) . putStrLn $
          "<- " ++ tshow origin ++ " - " ++ tshow msg
        return (Nothing, handleLogging)
