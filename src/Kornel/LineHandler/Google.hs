module Kornel.LineHandler.Google
  ( setup
  , google
  ) where

import qualified Data.Attoparsec.Text            as P
import           Kornel.Common
import           Kornel.Config
import           Kornel.LineHandler
import           Kornel.LineHandler.HttpSnippets (announceUrl)
import qualified Network.HTTP.Client.TLS         as HTTPS
import           Network.HTTP.Simple
import qualified Network.URI.Encode              as URI
import           Prelude                         hiding (Handler, handle)
import           Text.Regex.PCRE

setup :: Config -> HandlerRaw
setup cfg =
  withHelp cmdHelp . onlySimple . pure $ \respond _ request ->
    case parseMaybe cmdParser request of
      Nothing -> pure ()
      Just query ->
        asyncWithLog "Google" $
        google query >>=
        mapM_
          (\url -> do
             respond (Privmsg url)
             announceUrl cfg respond url)

cmdParser :: P.Parser Text
cmdParser = P.skipSpace *> P.asciiCI "@google" *> skipSpace1 *> P.takeText

cmdHelp :: Text
cmdHelp = "@google <query>"

google :: Text -> IO [Text]
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
  pure . take 1 . findUrls $ response

findUrls :: ByteString -> [Text]
findUrls input =
  filter
    (not . ("http://webcache.googleusercontent.com/search" `isPrefixOf`))
    urls
  where
    urls :: [Text] =
      mapMaybe
        (\case
           [_, a] -> Just (URI.decodeBSToText a)
           _ -> Nothing)
        urlMatches
    urlMatches :: [[ByteString]] =
      input =~ ("(?i)href=\"/url\\?q=([^&]+)" :: ByteString)
