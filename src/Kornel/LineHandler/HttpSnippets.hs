module Kornel.LineHandler.HttpSnippets
  ( handle
  ) where

import Control.Monad
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Kornel.CLI
import Kornel.LineHandler
import Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as HTTPS
import Text.Regex.PCRE

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
  request <- fakeChromium <$> parseRequest (T.unpack url)
  response <-
    withResponse request manager $ \r -> brReadSome (responseBody r) atMost
  let title = findTitle $ LBS.toStrict response
  return $ strip . decodeHtmlEntities . decodeUtf8_ <$> title

findTitle :: ByteString -> Maybe ByteString
findTitle haystack = listToMaybe $ Prelude.drop 1 matches
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
