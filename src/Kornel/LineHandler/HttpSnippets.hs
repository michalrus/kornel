module Kornel.LineHandler.HttpSnippets
  ( setup
  ) where

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import           Kornel.Common
import           Kornel.Config
import           Kornel.LineHandler
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as HTTPS
import           Prelude                 hiding (Handler, handle)
import           Text.Regex.PCRE

setup :: Config -> HandlerRaw
setup cfg =
  withHelp cmdHelp . onlyPrivmsgRespondWithNotice . pure $ \respond _ request ->
    case findURLs request of
      [] -> pure ()
      urls -> asyncWithLog "HttpSnippets" $ snippets cfg urls >>= mapM_ respond

cmdHelp :: Text
cmdHelp = "Snippets of posted URLs will be announced."

snippets :: Config -> [Text] -> IO (Maybe Text)
snippets cfg urls = do
  snips <- catMaybes <$> getSnippet (httpSnippetsFetchMax cfg) `traverse` urls
  return $
    case snips of
      [] -> Nothing
      xs -> Just $ T.intercalate "\n" xs

getSnippet :: Integer -> Text -> IO (Maybe Text)
getSnippet atMost url = do
  manager <- HTTPS.newTlsManager
  request <- setupUserAgent <$> parseRequest (unpack url)
  response <-
    withResponse request manager $ \r ->
      brReadSome (responseBody r) (fromIntegral atMost)
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
