module Kornel.Main
  ( main
  ) where

import qualified Data.ByteArray.Encoding         as B
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
import qualified Kornel.LineHandler.Money
import qualified Kornel.LineHandler.Scala
import qualified Kornel.LineHandler.Slap
import qualified Kornel.LineHandler.Wolfram
import qualified Kornel.Log                      as L
import qualified Network.Connection              as Net
import           Prelude                         hiding (Handler)
import qualified System.IO                       as IO

main :: IO ()
main = do
  mapM_ @[_] (`IO.hSetEncoding` IO.utf8) [stdout, stdin, stderr]
  readConfig >>= runConfig

runConfig :: Config -> IO ()
runConfig cfg = do
  L.log "Setting up handlers…"
  eventQueue <- newChan
  mConnection <- newMVar Nothing
  rate <- I.newRateLimit 1.0 4.0
  setupHandlers <-
    forM (allHandlers cfg) ($ (writeChan eventQueue . EClientLine))
  void .
    async .
    forever .
    handleAny
      ((>> (threadDelay $ 1 * 1000 * 1000)) .
       L.log . ("[ERROR] eventLoop: " ++) . tshow) $
    eventLoop cfg eventQueue setupHandlers mConnection rate
  -------
  ctx <- Net.initConnectionContext
  forever $ do
    L.log "Starting new connection…"
    finally
      (handleAny
         (L.log . ("[ERROR] runConnection: " ++) . tshow)
         (runConnection
            cfg
            ctx
            mConnection
            (writeChan eventQueue . EServerLine)
            (writeChan eventQueue . EClientLine)))
      (L.log "Connection ended.")
    threadDelay $ 10 * 1000 * 1000 -- µs

data EventQueue
  = EClientLine I.RawIrcMsg
  | EServerLine I.IrcMsg

-- |The order here constitutes the order in `@help`.
allHandlers :: Config -> [HandlerRaw]
allHandlers cfg = snd <$> input
  where
    input :: [(Help, HandlerRaw)]
    input =
      [ handlePing
      , Kornel.LineHandler.Slap.setup
      , handleBots
      , Kornel.LineHandler.Google.setup cfg
      , Kornel.LineHandler.Wolfram.setup cfg
      , Kornel.LineHandler.Clojure.setup
      , Kornel.LineHandler.Scala.setup cfg
      , Kornel.LineHandler.Haskell.setup cfg
      , Kornel.LineHandler.Money.setup
      , Kornel.LineHandler.HttpSnippets.setup cfg
      , Kornel.LineHandler.Chatter.setup cfg
      , helpHandler (fst <$> input)
      ]

eventLoop ::
     Config
  -> Chan EventQueue
  -> [I.IrcMsg -> IO ()]
  -> MVar (Maybe Net.Connection)
  -> I.RateLimit
  -> IO ()
eventLoop Config {logTraffic} eventQueue handlers mConnection rate =
  readChan eventQueue >>= \case
    EClientLine command ->
      readMVar mConnection >>= \case
        Nothing ->
          L.log $
          "[WARNING] No connection, command not delivered: " ++ tshow command
        Just connection -> do
          I.tickRateLimit rate
          when logTraffic . L.log $ "-> " ++ tshow command
          Net.connectionPut connection . I.renderRawIrcMsg $ command
    EServerLine msg -> do
      when logTraffic . L.log $ "<- " ++ tshow msg
      forM_
        handlers
        (handleAny (L.log . ("[ERROR] handler: " ++) . tshow) . ($ msg))

data PingTimeout =
  PingTimeout
  deriving (Eq, Show)

instance Exception PingTimeout

runConnection ::
     Config
  -> Net.ConnectionContext
  -> MVar (Maybe Net.Connection)
  -> (I.IrcMsg -> IO ())
  -> (I.RawIrcMsg -> IO ())
  -> IO ()
runConnection cfg ctx mConnection announceMsg sendCommand = do
  (connection, loginCommands) <- login cfg ctx
  finally
    (do void $ swapMVar mConnection (Just connection)
        mapM_ sendCommand loginCommands
        let pingEvery = 20 * 1000 * 1000 -- µs
        void $
          race
            (forever $ do
               threadDelay pingEvery
               uuid <- UUID.nextRandom
               sendCommand $ I.ircPing [UUID.toText uuid])
            (forever $
             timeout
               (2 * pingEvery)
               (takeWhile (/= 13) <$> Net.connectionGetLine 1024 connection) -- 13 is '\r'
              >>= \case
               Nothing -> throwIO PingTimeout
               Just rawBytes ->
                 case map I.cookIrcMsg . I.parseRawIrcMsg . I.asUtf8 $ rawBytes of
                   Just a -> announceMsg a
                   Nothing ->
                     L.log $
                     "[ERROR] Failed to parse message " ++ tshow rawBytes ++ "."))
    (do void $ swapMVar mConnection Nothing
        Net.connectionClose connection)

login :: Config -> Net.ConnectionContext -> IO (Net.Connection, [I.RawIrcMsg])
login cfg ctx = do
  con <-
    Net.connectTo
      ctx
      Net.ConnectionParams
        { connectionHostname = serverHost cfg
        , connectionPort = serverPort cfg
        , connectionUseSecure =
            if not $ usingSSL cfg
              then Nothing
              else Just
                     Net.TLSSettingsSimple
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
  return
    (con, saslPrefixCmd ++ helloCmd ++ saslAuthCmd ++ nickservCmd ++ joinCmd)

handlePing :: (Help, HandlerRaw)
handlePing =
  ( Help []
  , \sendMsg ->
      pure $ \case
        I.Ping ts -> sendMsg $ I.ircPong ts
        _ -> pure ())

handleBots :: (Help, HandlerRaw)
handleBots =
  (Help [(["bots"], "")], ) . onlySimple $
  pure
    (\respond _ ->
       \case
         "@bots" -> respond . Privmsg $ ":)"
         _ -> pure ())
