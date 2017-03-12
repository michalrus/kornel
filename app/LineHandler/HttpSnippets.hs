module LineHandler.HttpSnippets
       ( handle
       , snippets
       ) where

import LineHandler
import Control.Monad
import Data.ByteString
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Regex.PCRE
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LBS

-- | This many bytes will be read from each snippet, until a <title/> is found.
readAtMostBytes :: Int
readAtMostBytes = 2048

handle :: LineHandler
handle = onlyPrivmsgRespondWithNotice handleP
  where
    handleP = Handler $ \_ (_, _, msg) -> do
      res <- join <$> (discardException $ snippets msg)
      return (res, handleP)

snippets :: Text -> IO (Maybe Text)
snippets text = do
  let urls = findURLs text
  snips <- catMaybes <$> getSnippet `traverse` urls
  return $ case snips of
    [] -> Nothing
    xs -> Just $ T.intercalate "\n" xs

getSnippet :: Text -> IO (Maybe Text)
getSnippet url = do
  manager <- HTTPS.newTlsManager
  request <- parseRequest $ T.unpack url
  response <- withResponse request manager $ \r ->
    brReadSome (responseBody r) readAtMostBytes
  let title = findTitle $ LBS.toStrict response
  return $ decodeUtf8 <$> title

findTitle :: ByteString -> Maybe ByteString
findTitle haystack =
  listToMaybe $ Prelude.drop 1 matches
  where
    matches :: [ByteString] = getAllTextSubmatches $ haystack =~ ("(?i)<title>([^<]+)" :: ByteString)

findURLs :: Text -> [Text]
findURLs input =
  decodeUtf8 <$> matches
  where
    inputBS = encodeUtf8 input
    matches :: [ByteString] = getAllTextMatches $ inputBS =~ ("(?i)https?://[^\\s><\\]\\[]+" :: ByteString)
