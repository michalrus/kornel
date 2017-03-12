module LineHandler.Google
       ( handle
       , google
       ) where

import LineHandler
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
      response <- join <$> (discardException $ join <$> google `traverse` query)
      return (response, handleP)

cmdParser :: Parser Text
cmdParser = skipSpace *> asciiCI "@google" *> skip isHorizontalSpace *> takeText

google :: Text -> IO (Maybe Text)
google query = do
  manager <- HTTPS.newTlsManager
  let request
        = setRequestManager manager
        $ addRequestHeader "Accept-Language" "en-US,en;q=0.8"
        $ addRequestHeader "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
        $ addRequestHeader "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"
        $ setRequestQueryString [ ("q",  Just $ encodeUtf8 query)
                                , ("hl", Just "en")
                                , ("ie", Just "UTF-8")
                                ]
        $ "https://www.google.com/search"
  response <- LBS.toStrict <$> getResponseBody <$> httpLBS request
  let result = firstResult response
  return $ (\(GResult u t) -> "“" <> t <> "” — " <> u) <$> result

data GResult = GResult
               { url :: Text
               , title :: Text
               } deriving (Show)

firstResult :: BS.ByteString -> Maybe GResult
firstResult input =
  case parts of
    _ : u : t : [] -> Just $ GResult { url = decodeUtf8 u, title = decodeUtf8 t }
    _              -> Nothing
  where
    first :: ByteString = input =~ ("(?i)<h3.*?</h3>" :: ByteString)
    parts :: [ByteString] = getAllTextSubmatches $ first =~ ("(?i)<a .*?href=\"(.*?)\".*?>(.*?)<" :: ByteString)
