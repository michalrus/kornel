module Kornel.LineHandler.HttpSnippets
  ( handle
  ) where

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import           Kornel.CLI
import           Kornel.LineHandler
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as HTTPS
import           Prelude                 hiding (Handler, handle)
import           Text.Regex.PCRE

handle :: LineHandler
handle = onlyPrivmsgRespondWithNotice handleP
  where
    handleP =
      Handler $ \cfg (_, _, msg) -> do
        res <- join <$> discardException (snippets cfg msg)
        return (res, handleP)

snippets :: Config -> Text -> IO (Maybe Text)
snippets cfg text = do
  let urls = findURLs text
  snips <- catMaybes <$> getSnippet (httpSnippetsFetchMax cfg) `traverse` urls
  return $
    case snips of
      [] -> Nothing
      xs -> Just $ T.intercalate "\n" xs

getSnippet :: Int -> Text -> IO (Maybe Text)
getSnippet atMost url = do
  manager <- HTTPS.newTlsManager
  request <- setupUserAgent <$> parseRequest (unpack url)
  response <-
    withResponse request manager $ \r -> brReadSome (responseBody r) atMost
  let title = findTitle $ LBS.toStrict response
  return $ T.strip . decodeHtmlEntities . decodeUtf8_ <$> title

findTitle :: ByteString -> Maybe ByteString
findTitle haystack = headMay $ drop 1 matches
  where
    matches :: [ByteString] =
      getAllTextSubmatches $
      haystack =~ ("(?i)<title(?: [^>]+)?>([^<]+)" :: ByteString)

findURLs :: Text -> [Text]
findURLs input = decodeUtf8_ <$> matches
  where
    inputBS = encodeUtf8 input
    matches :: [ByteString] =
      getAllTextMatches $
      inputBS =~ ("(?i)https?://[^\\s><\\]\\[]+" :: ByteString)
