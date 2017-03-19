module Kornel.LineHandler.Google
       ( handle
       , google
       ) where

import Kornel.LineHandler
import Control.Monad
import Data.Attoparsec.Text as P
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString as BS
import Network.HTTP.Simple
import Text.Regex.PCRE

handle :: LineHandler
handle = onlyPrivmsg handleP
  where
    handleP = Handler $ \_ (_, _, msg) -> do
      let query = runParser cmdParser msg
      response <- join <$> discardException (join <$> google `traverse` query)
      return (response, handleP)

cmdParser :: Parser Text
cmdParser = skipSpace *> asciiCI "@google" *> skip isHorizontalSpace *> takeText

google :: Text -> IO (Maybe Text)
google query = do
  manager <- HTTPS.newTlsManager
  let request
        = setRequestManager manager
        . fakeChromium
        . setRequestQueryString [ ("q",  Just $ encodeUtf8 query)
                                , ("hl", Just "en")
                                , ("ie", Just "UTF-8")
                                ]
        $ "https://www.google.com/search"
  response <- LBS.toStrict . getResponseBody <$> httpLBS request
  let result = firstResult response
  return $ (\(GResult u t) -> "“" <> t <> "” — " <> u) <$> result

data GResult = GResult
               { url :: Text
               , title :: Text
               } deriving (Show)

firstResult :: BS.ByteString -> Maybe GResult
firstResult input =
  case parts of
    [_, u, t] -> Just GResult { url = strip $ decodeUtf8 u, title = strip $ decodeHtmlEntities $ decodeUtf8 t }
    _         -> Nothing
  where
    first :: ByteString = input =~ ("(?i)<h3.*?</h3>" :: ByteString)
    parts :: [ByteString] = getAllTextSubmatches $ first =~ ("(?i)<a .*?href=\"(.*?)\".*?>(.*?)<" :: ByteString)
