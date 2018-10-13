module Kornel.LineHandler.Wolfram
  ( setup
  ) where

import qualified Data.Attoparsec.Text    as P
import           Kornel.Common
import           Kornel.Config
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

setup :: Config -> (Help, HandlerRaw)
setup cfg =
  (cmdHelp, ) . onlySimple . pure $ \respond _ request ->
    forM_ (parseMaybe cmdParser request) $ \expr ->
      asyncWithLog "Wolfram" $ wolfram cfg expr >>= mapM_ (respond . Privmsg)

cmdParser :: P.Parser Text
cmdParser = P.skipSpace *> P.asciiCI "@wolfram" *> skipSpace1 *> P.takeText

cmdHelp :: Help
cmdHelp = Help [(["wolfram"], "<query>")]

wolfram :: Config -> Text -> IO (Maybe Text)
wolfram Config {wolframApiKey} expr = do
  manager <- HTTPS.newTlsManager
  let request =
        setRequestManager manager .
        setRequestQueryString
          [ ("appid", encodeUtf8 <$> wolframApiKey)
          , ("i", Just . encodeUtf8 $ expr)
          , ("units", Just "metric")
          ] $
        "http://api.wolframalpha.com/v1/result"
  response :: Text <-
    decodeUtf8 . toStrict . getResponseBody <$> httpLBS request
  pure . Just $ response
