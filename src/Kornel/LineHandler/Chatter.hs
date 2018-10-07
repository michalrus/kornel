module Kornel.LineHandler.Chatter
  ( handle
  ) where

import           Data.Aeson
import           Data.Attoparsec.Text    as P
import           Data.Coerce
import           Kornel.Config
import qualified Kornel.IrcParser        as I
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

data HState = HState
  { cleverState :: Maybe Text
  , apiKey :: Maybe Text
  }

{-# ANN CleverbotResponse ("HLint: ignore Use camelCase" :: String)
        #-}

data CleverbotResponse = CleverbotResponse
  { cs :: Text
  , clever_output :: Text
  } deriving (Show, Generic)

instance FromJSON CleverbotResponse

handle :: LineHandler
handle = onlyPrivmsg . handle' $ HState Nothing Nothing
  where
    handle' :: HState -> PrivmsgHandler
    handle' state =
      Handler $ \cfg (origin, _, msg) ->
        let isToMe = toUpper myNick `isInfixOf` toUpper msg
              where
                myNick = coerce $ nick cfg
            highlight t = theirNick ++ ": " ++ t
              where
                theirNick = coerce $ I.nick origin
         in if isToMe
              then do
                let question =
                      fromMaybe msg $ parseMaybe (stripHighlight $ nick cfg) msg
                stateWithKey <- tryToLoadKey cfg state
                (nextState, answer) <-
                  fromMaybe (state, Nothing) <$>
                  discardException (chatter stateWithKey question)
                return (highlight <$> answer, handle' nextState)
              else return (Nothing, handle' state)

tryToLoadKey :: Config -> HState -> IO HState
tryToLoadKey cfg state =
  if isJust $ apiKey state
    then return state
    else return $ state {apiKey = cleverBotApiKey cfg}

stripHighlight :: I.Target -> Parser Text
stripHighlight myNick =
  skipSpace *> asciiCI (coerce myNick) *> skipSpace *>
  optional (char ':' <|> char ',') *>
  skip isHorizontalSpace *>
  takeText

chatter :: HState -> Text -> IO (HState, Maybe Text)
chatter state msg = do
  manager <- HTTPS.newTlsManager
  let request =
        setRequestManager manager .
        setRequestQueryString
          [ ("key", encodeUtf8 <$> apiKey state)
          , ("cs", encodeUtf8 <$> cleverState state)
          , ("input", Just $ encodeUtf8 msg)
          ] $
        "https://www.cleverbot.com/getreply"
  response <- discardException $ getResponseBody <$> httpJSON request
  return
    ( maybe state (\r -> state {cleverState = Just $ cs r}) response
    , clever_output <$> response)
