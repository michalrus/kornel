module Kornel.LineHandler.Chatter
  ( setup
  ) where

import           Data.Aeson              (FromJSON)
import           Data.Attoparsec.Text    as P
import           Data.Coerce
import qualified Irc.Identifier          as I
import qualified Irc.UserInfo            as I
import           Kornel.Common
import           Kornel.Config
import           Kornel.LineHandler
import qualified Kornel.Log              as L
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

newtype HState =
  HState (Maybe Text)

{-# ANN CleverbotResponse ("HLint: ignore Use camelCase" :: String)
        #-}

setup :: Config -> HandlerRaw
setup cfg =
  withHelp cmdHelp . onlySimple $ do
    state <- newTVarIO (HState Nothing)
    pure $ \respond origin request ->
      let isToMe = toUpper myNick `isInfixOf` toUpper request
            where
              myNick = I.idText $ nick cfg
          highlight t = theirNick ++ ": " ++ t
            where
              theirNick = I.idText $ I.userNick origin
       in when isToMe . asyncWithLog "Chatter" $ do
            let question =
                  fromMaybe request $
                  parseMaybe (stripHighlight $ nick cfg) request
            answer <- chatter cfg state question
            respond . Privmsg . highlight $ answer

cmdHelp :: Text
cmdHelp = "(You can also just talk to me.)"

stripHighlight :: I.Identifier -> Parser Text
stripHighlight myNick =
  skipSpace *> asciiCI (I.idText myNick) *> skipSpace *>
  optional (char ':' <|> char ',') *>
  skip isHorizontalSpace *>
  takeText

data CleverbotResponse = CleverbotResponse
  { cs :: Text
  , clever_output :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CleverbotResponse

chatter :: Config -> TVar HState -> Text -> IO Text
chatter Config {cleverBotApiKey} state msg =
  catchAny
    go
    (\ex -> do
       L.log $ "[WARNING] Kornel.Chatter: error (resetting state): " ++ tshow ex
       -- Couteract the weird API error when after a longer time it
       -- returns responses with invalid Unicode in `conversation_id`.
       atomically . writeTVar state . HState $ Nothing
       go)
  where
    go = do
      state' <- readTVarIO state
      manager <- HTTPS.newTlsManager
      let request =
            setRequestManager manager .
            setRequestQueryString
              [ ("key", encodeUtf8 <$> cleverBotApiKey)
              , ("cs", encodeUtf8 <$> coerce state')
              , ("input", Just $ encodeUtf8 msg)
              ] $
            "https://www.cleverbot.com/getreply"
      response <- getResponseBody <$> httpJSON request
      atomically . writeTVar state . HState . Just . cs $ response
      pure . clever_output $ response
