module Kornel.LineHandler.Chatter
  ( handle
  ) where

import Control.Applicative
import Control.Monad
import Control.Newtype as N
import Data.Aeson
import Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe, maybe, isJust)
import Data.Monoid ((<>))
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Traversable
import GHC.Generics
import qualified IrcParser as I
import Kornel.CLI
import Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Simple

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
handle = onlyPrivmsg $ handle' $ HState Nothing Nothing
  where
    handle' :: HState -> PrivmsgHandler
    handle' state =
      Handler $ \cfg (origin, _, msg) ->
        let isToMe = toUpper myNick `isInfixOf` toUpper msg
              where
                myNick = N.unpack $ nick cfg
            highlight t = theirNick <> ": " <> t
              where
                theirNick = N.unpack $ I.nick origin
        in if isToMe
             then do
               let question =
                     fromMaybe msg $ runParser (stripHighlight $ nick cfg) msg
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
    else do
      cbApiKey <-
        join <$>
        discardException
          (for (cleverBotApiKeyFile cfg) $ \p -> strip <$> TIO.readFile p)
      return $ state {apiKey = cbApiKey}

stripHighlight :: I.Target -> Parser Text
stripHighlight myNick =
  skipSpace *> asciiCI (N.unpack myNick) *> skipSpace *>
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
