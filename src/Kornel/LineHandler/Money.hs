module Kornel.LineHandler.Money
  ( setup
  ) where

import qualified Data.Attoparsec.Text    as P
import           Data.Char               (isSpace)
import qualified Data.Map                as Map
import           Kornel.Common
import           Kornel.LineHandler
import qualified Network.HTTP.Client.TLS as HTTPS
import           Network.HTTP.Simple
import           Prelude                 hiding (Handler, handle)

setup :: HandlerRaw
setup =
  withHelp cmdHelp . onlySimple . pure $ \respond _ request ->
    case parseMaybe cmdParser request of
      Nothing -> pure ()
      Just expr ->
        asyncWithLog "Money" $ money expr >>= mapM_ (respond . Privmsg)

cmdParser :: P.Parser (Double, Text, [Text])
cmdParser = do
  P.skipSpace <* (P.asciiCI "@money" <|> P.asciiCI "@currency")
  amount <- skipSpace1 *> P.double
  from <- symb
  skipSpace1 <* (P.asciiCI "to" <|> P.asciiCI "in")
  to <- P.many1 symb
  pure (amount, from, to)
  where
    symb = toUpper <$> (skipSpace1 *> P.takeWhile1 (not . isSpace))

cmdHelp :: Text
cmdHelp =
  "{ @money | @currency } <amount> <from-symb> { to | in } <to-symb1>[ <to-symb2> …]"

money :: (Double, Text, [Text]) -> IO (Maybe Text)
money (amount, from, to) = do
  manager <- HTTPS.newTlsManager
  let request =
        setRequestManager manager .
        setRequestQueryString
          [ ("fsym", Just . encodeUtf8 $ from)
          , ("tsyms", Just . encodeUtf8 . intercalate "," $ to)
          ] $
        "https://min-api.cryptocompare.com/data/price"
  response :: Map Text Double <- getResponseBody <$> httpJSON request
  pure . Just . render $ response
  where
    render :: Map Text Double -> Text
    render rates =
      tshow amount ++
      " " ++
      from ++
      " is " ++
      (intercalate ", or " .
       map (\(symb, rate) -> tshow (rate * amount) ++ " " ++ symb) . Map.toList $
       rates) ++
      "."
