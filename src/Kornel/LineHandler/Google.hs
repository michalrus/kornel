module Kornel.LineHandler.Google
  ( handle
  , google
  ) where

import qualified Data.Attoparsec.Text    as P
import qualified Data.Text               as T
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)
import           Text.Regex.PCRE

handle :: LineHandler
handle = onlyPrivmsg handleP
  where
    handleP =
      Handler $ \_ (_, _, msg) -> do
        let query = parseMaybe cmdParser msg
        response <- join <$> discardException (join <$> google `traverse` query)
        return (response, handleP)

cmdParser :: P.Parser Text
cmdParser =
  P.skipSpace *> P.asciiCI "@google" *> P.skip P.isHorizontalSpace *> P.takeText

google :: Text -> IO (Maybe Text)
google query = do
  manager <- HTTPS.newTlsManager
  let request =
        setRequestManager manager .
        setupUserAgent .
        setRequestQueryString
          [ ("q", Just $ encodeUtf8 query)
          , ("hl", Just "en")
          , ("ie", Just "UTF-8")
          ] $
        "https://www.google.com/search"
  response <- toStrict . getResponseBody <$> httpLBS request
  let result = firstResult response
  return $ (\(GResult u t) -> "“" ++ t ++ "” — " ++ u) <$> result

data GResult = GResult
  { url :: Text
  , title :: Text
  } deriving (Show)

firstResult :: ByteString -> Maybe GResult
firstResult input =
  case parts of
    [_, u, t] ->
      Just
        GResult
          { url = T.strip $ decodeUtf8_ u
          , title = T.strip . decodeHtmlEntities . decodeUtf8_ $ t
          }
    _ -> Nothing
  where
    first' :: ByteString = input =~ ("(?i)<h3.*?</h3>" :: ByteString)
    parts :: [ByteString] =
      getAllTextSubmatches $
      first' =~ ("(?i)<a .*?href=\"(.*?)\".*?>(.*?)<" :: ByteString)
