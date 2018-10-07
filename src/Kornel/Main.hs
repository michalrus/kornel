module Kornel.Main
  ( main
  ) where

import qualified Data.ByteArray.Encoding         as B

import qualified Control.Concurrent              as Unsafe
import           Control.Monad.Loops             (whileJust_)
import qualified Data.Text                       as T
import qualified Data.UUID                       as UUID
import qualified Data.UUID.V4                    as UUID
import           GHC.Conc                        (threadDelay)
import qualified Irc.Commands                    as I
import qualified Irc.Identifier                  as I
import qualified Irc.Message                     as I
import qualified Irc.RateLimit                   as I
import qualified Irc.RawIrcMsg                   as I
import           Kornel.Config
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
import           System.IO.Error                 (catchIOError)

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
  | QClientLine I.RawIrcMsg
  | QServerLine I.IrcMsg
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
  rate <- I.newRateLimit 2.0 8.0
  con <- login cfg rate ctx
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
      writeChan ipc . QClientLine . I.ircPing $ [UUID.toText uuid]
  -- process the queue
  void . discardException . iterateWhileJust (pure allHandlers) $
    processQueue cfg rate con (readChan ipc) (writeChan ipc)
  connectionClose con
  Unsafe.killThread thrPing
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
  -> I.RateLimit
  -> Connection
  -> IO IpcQueue
  -> (IpcQueue -> IO ())
  -> [LineHandler]
  -> IO (Maybe [LineHandler])
processQueue cfg rate con dequeue enqueue handlers =
  dequeue >>= \case
    QQuit -> return Nothing
    QUpdateHandler handlerId new ->
      return . Just $ editNth handlers handlerId new
    QClientLine cmd -> do
      sendCommand cfg rate con cmd
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

login :: Config -> I.RateLimit -> ConnectionContext -> IO Connection
login cfg rate ctx = do
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
  let user = I.idText . nick $ cfg
      helloCmd =
        [ I.ircNick user
        , I.ircUser user False False "+https://github.com/michalrus/kornel"
        ]
      saslPrefixCmd = maybe [] (const [I.ircCapLs]) (saslPassword cfg)
      saslAuthCmd =
        maybe
          []
          (\pass ->
             [ I.ircCapReq ["sasl"]
             , I.ircAuthenticate I.plainAuthenticationMode
             , I.ircAuthenticate
                 (decodeUtf8 . B.convertToBase B.Base64 . encodeUtf8 $
                  user ++ "\0" ++ user ++ "\0" ++ pass)
             , I.ircCapEnd
             ])
          (saslPassword cfg)
      nickservCmd =
        toList
          (I.ircPrivmsg "NickServ" . T.append "IDENTIFY " . T.strip <$>
           nickservPassword cfg)
      joinCmd = flip I.ircJoin Nothing . I.idText <$> channels cfg
  mapM_ (sendCommand cfg rate con) $
    saslPrefixCmd ++ helloCmd ++ saslAuthCmd ++ nickservCmd ++ joinCmd
  return con

processRawLine :: Connection -> IO (Maybe I.IrcMsg)
processRawLine con =
  flip
    catchIOError
    (\e ->
       if isEOFError e
         then return Nothing
         else ioError e) $ do
    rawBytes <- takeWhile (/= 13) <$> connectionGetLine 1024 con -- 13 is '\r'
    case map I.cookIrcMsg . I.parseRawIrcMsg . I.asUtf8 $ rawBytes of
      Nothing -> do
        IO.hPutStrLn stderr $ "Failed to parse message " ++ show rawBytes
        return Nothing
      a -> return a

sendCommand :: Config -> I.RateLimit -> Connection -> I.RawIrcMsg -> IO ()
sendCommand Config {logTraffic} rate con msg = do
  I.tickRateLimit rate
  when logTraffic . putStrLn $ "-> " ++ tshow msg
  connectionPut con . I.renderRawIrcMsg $ msg

handlePing :: LineHandler
handlePing =
  Handler $ \_ ->
    \case
      I.Ping ts -> return (Just $ I.ircPong ts, handlePing)
      _ -> return (Nothing, handlePing)

handleLogging :: LineHandler
handleLogging =
  Handler $ \Config {logTraffic} ->
    \case
      msg -> do
        when logTraffic . putStrLn $ "<- " ++ tshow msg
        return (Nothing, handleLogging)
