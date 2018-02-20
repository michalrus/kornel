module Kornel.LineHandler.Clojure
  ( handle
  ) where

import           Data.Aeson
import qualified Data.Attoparsec.Text    as P
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

handle :: LineHandler
handle = onlyPrivmsg handleP
  where
    handleP =
      Handler $ \_ (_, _, msg) -> do
        let sexpr = runParser cmdParser msg
        res <- join <$> discardException (join <$> eval `traverse` sexpr)
        return (res, handleP)

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
