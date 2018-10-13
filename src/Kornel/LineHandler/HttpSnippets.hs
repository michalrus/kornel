module Kornel.LineHandler.HttpSnippets
  ( setup
  , announceUrl
  ) where

import           Data.Aeson              (FromJSON)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as T
import           Kornel.Common
import           Kornel.Config
import           Kornel.LineHandler
import           Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple     hiding (withResponse)
import           Prelude                 hiding (Handler, handle)
import           Text.Regex.PCRE

setup :: Config -> (Help, HandlerRaw)
setup cfg =
  (cmdHelp, ) . onlySimple . pure $ \respond _ request ->
    forM_ (findURLs request) (announceUrl cfg respond)

announceUrl :: Config -> (SimpleReply -> IO ()) -> Text -> IO ()
announceUrl cfg respond url =
  asyncWithLog "HttpSnippets.title" $ do
    title cfg url >>= mapM_ (respond . Notice)
    asyncWithLog "HttpSnippets.smmry" $
      smmry cfg url >>= mapM_ (respond . Notice)

cmdHelp :: Help
cmdHelp = Help [([], "Snippets of posted URLs will be announced.")]

smmry :: Config -> Text -> IO (Maybe Text)
smmry Config {smmryApiKey} url =
  map join . forM smmryApiKey $ \apiKey -> do
    manager <- HTTPS.newTlsManager
    let request =
          setRequestManager manager .
          setupUserAgent .
          setRequestQueryString
            [ ("SM_API_KEY", Just $ encodeUtf8 apiKey)
            , ("SM_LENGTH", Just "1")
            , ("SM_URL", Just $ encodeUtf8 url)
            ] $
          "https://api.smmry.com/"
    SmmryResponse {sm_api_content} <- getResponseBody <$> httpJSON request
    pure . map T.strip $ sm_api_content

newtype SmmryResponse = SmmryResponse
  { sm_api_content :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON SmmryResponse

title :: Config -> Text -> IO (Maybe Text)
title Config {httpSnippetsFetchMax} url = do
  manager <- HTTPS.newTlsManager
  request <- setupUserAgent <$> parseRequest (unpack url)
  response <-
    withResponse request manager $ \r ->
      brReadSome (responseBody r) (fromIntegral httpSnippetsFetchMax)
  let title' = findTitle $ LBS.toStrict response
  return $ T.strip . decodeHtmlEntities . decodeUtf8_ <$> title'

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
