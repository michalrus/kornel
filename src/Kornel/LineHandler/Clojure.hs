module Kornel.LineHandler.Clojure
  ( setup
  ) where

import           Data.Aeson
import qualified Data.Attoparsec.Text    as P
import           Kornel.Common
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

setup :: HandlerRaw
setup =
  onlyPrivmsg . pure $ \respond _ request ->
    case parseMaybe cmdParser request of
      Nothing -> pure ()
      Just sexpr -> asyncWithLog "Clojure" $ eval sexpr >>= mapM_ respond

cmdParser :: P.Parser Text
cmdParser =
  P.skipSpace *> (P.asciiCI "@clojure" <|> P.asciiCI "@clj") *>
  P.skip P.isHorizontalSpace *>
  P.takeText

data TryCljResponse = TryCljResponse
  { result :: Maybe Text
  , message :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TryCljResponse

eval :: Text -> IO (Maybe Text)
eval sexpr = do
  manager <- HTTPS.newTlsManager
  let request =
        setRequestManager manager .
        setRequestQueryString [("expr", Just $ encodeUtf8 sexpr)] $
        "http://www.tryclj.com/eval.json"
  response <- getResponseBody <$> httpJSON request
  return (render <$> response)
  where
    render :: TryCljResponse -> Text
    render r =
      flip fromMaybe (message r) $
      fromMaybe "Error: got neither a ‘result’ nor a ‘message’." (result r)
