module Kornel.LineHandler
  ( Handler(..)
  , emptyHandler
  , LineHandler
  , PrivmsgHandler
  , onlyPrivmsg
  , onlyPrivmsgRespondWithNotice
  , randomElem
  , meAction
  , runParser
  , discardException
  , discardError
  , fakeChromium
  , decodeHtmlEntities
  , decodeUtf8_
  ) where

import qualified Control.Exception.Base as E
import Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Profunctor
import Data.Text
import Data.Text.Encoding (decodeUtf8With)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import HTMLEntities.Decoder (htmlEncodedText)
import qualified IrcParser as I
import qualified Kornel.CLI as C
import Network.HTTP.Client (Request, requestHeaders)
import System.IO
import System.Random (randomRIO)

newtype Handler a b =
  Handler (C.Config -> a -> IO (Maybe b, Handler a b))

instance Profunctor Handler where
  dimap f g (Handler fab) =
    Handler $ \cfg c -> do
      (mb, next) <- fab cfg (f c)
      return (g <$> mb, dimap f g next)

-- FIXME: is there a class for that?
-- dimapMaybe :: (c -> a) -> (b -> Maybe d) -> Handler a b -> Handler c d
-- dimapMaybe f g (Handler fab) =
--   Handler $ \cfg c -> do
--     (mb, next) <- fab cfg (f c)
--     return (g =<< mb, dimapMaybe f g next)
emptyHandler :: Handler a b
emptyHandler = Handler $ \_ _ -> return (Nothing, emptyHandler)

type LineHandler = Handler I.IrcLine I.IrcCommand

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.Origin, I.Target, Text) Text

-- FIXME: how to use Profunctor’s dimap to implement this?
onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg (Handler handler) =
  Handler $ \cfg ->
    \case
      I.IrcLine (Just origin) (I.Privmsg target msg) -> do
        (response, newHandler) <- handler cfg (origin, target, msg)
        let replyTo =
              if I.isChannel target
                then target
                else I.nick origin
        let realResponse = I.Privmsg replyTo <$> response
        return (realResponse, onlyPrivmsg newHandler)
      _ -> return (Nothing, onlyPrivmsg $ Handler handler)

onlyPrivmsgRespondWithNotice :: PrivmsgHandler -> LineHandler
onlyPrivmsgRespondWithNotice privmsg = dimap id g $ onlyPrivmsg privmsg
  where
    g (I.Privmsg t m) = I.Notice t m
    g a = a

randomElem :: [a] -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem xs = Just . (!!) xs <$> randomRIO (0, Prelude.length xs - 1)

meAction :: Text -> Text
meAction act = "\001ACTION " <> act <> "\001"

runParser :: Parser a -> Text -> Maybe a
runParser p t = discardError $ parseOnly p t

discardException :: IO a -> IO (Maybe a)
discardException action =
  E.catch (Just <$> action) $ \(e :: E.SomeException) -> do
    hPutStrLn stderr $ "Error: " ++ show e
    return Nothing

discardError :: Either a b -> Maybe b
discardError =
  \case
    Left _ -> Nothing
    Right b -> Just b

fakeChromium :: Request -> Request
fakeChromium r =
  r
  { requestHeaders =
      [ ("Accept-Language", "en-US,en;q=0.8")
      , ( "Accept"
        , "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
      , ( "User-Agent"
        , "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36")
      ]
  }

decodeHtmlEntities :: Text -> Text
decodeHtmlEntities = TL.toStrict . TLB.toLazyText . htmlEncodedText

decodeUtf8_ :: ByteString -> Text
decodeUtf8_ = decodeUtf8With (\_ _ -> Just '_')
