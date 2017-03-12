module LineHandler.Chatter
       ( handle
       ) where

import System.IO
import GHC.Generics
import LineHandler
import Control.Applicative
import Control.Monad
import qualified Control.Exception.Base as E
import Data.Aeson
import Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe, maybe, isJust)
import Data.Monoid ((<>))
import Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Simple
import qualified IrcParser as I
import CLI

data HState = HState
              { cleverState :: Maybe Text
              , apiKey :: Maybe Text
              }

data CleverbotResponse = CleverbotResponse
                         { cs :: Text
                         , clever_output :: Text
                         } deriving (Show, Generic)

instance FromJSON CleverbotResponse

discardException :: IO a -> IO (Maybe a)
discardException action =
  E.catch (Just <$> action) $ \(e :: E.SomeException) -> do
    hPutStrLn stderr $ "Error: Chatter: " ++ show e
    return Nothing

handle :: LineHandler
handle = onlyPrivmsg $ handleP $ HState Nothing Nothing

handleP :: HState -> PrivmsgHandler
handleP state = Handler $ \cfg (hostmask, _, msg) ->
  if (toUpper $ nick cfg) `isInfixOf` (toUpper $ msg) then do
    let question = fromMaybe msg $ runParser (noHighlight $ nick cfg) msg
    stateWithKey <- tryToLoadKey cfg state
    (nextState, answer) <- (fromMaybe (state, Nothing)) <$> (discardException $ chatter stateWithKey question)
    let highlight t = (I.nick hostmask) <> ": " <> t
    return (highlight <$> answer, handleP nextState)
  else return (Nothing, handleP state)

tryToLoadKey :: Config -> HState -> IO HState
tryToLoadKey cfg state = if isJust $ apiKey state then return state else do
  cbApiKey <- join <$> (discardException
                         $ flip traverse (cleverBotApiKeyFile cfg)
                         $ \p -> strip <$> TIO.readFile p)
  return $ state { apiKey = cbApiKey }

noHighlight :: Text -> Parser Text
noHighlight myNick =
  skipSpace *> asciiCI myNick *> skipSpace *> optional (char ':' <|> char ',')
  *> skip isHorizontalSpace *> takeText

chatter :: HState -> Text -> IO (HState, Maybe Text)
chatter state msg = do
  manager <- HTTPS.newTlsManager
  let request
        = setRequestManager manager
        $ setRequestQueryString [ ("key",   encodeUtf8 <$> apiKey state)
                                , ("cs",    encodeUtf8 <$> cleverState state)
                                , ("input", Just $ encodeUtf8 msg)
                                ]
        $ "https://www.cleverbot.com/getreply"
  response <- discardException $ getResponseBody <$> httpJSON request
  return (maybe state (\r -> state { cleverState = Just $ cs r }) response, clever_output <$> response)
