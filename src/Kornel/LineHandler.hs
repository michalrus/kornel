module Kornel.LineHandler
  ( Handler(..)
  , emptyHandler
  , LineHandler
  , PrivmsgHandler
  , onlyPrivmsg
  , onlyPrivmsgRespondWithNotice
  , randomElem
  , meAction
  , parseMaybe
  , discardException
  , isChannelIdentifier
  , eitherToMaybe
  , setupUserAgent
  , decodeHtmlEntities
  , decodeUtf8_
  ) where

import qualified Control.Exception.Base as E
import           Data.Attoparsec.Text   as P
import           Data.ByteString        (ByteString)
import qualified Data.List              as Unsafe
import           Data.Text.Encoding     (decodeUtf8With)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           HTMLEntities.Decoder   (htmlEncodedText)
import qualified Irc.Commands           as I
import qualified Irc.Identifier         as I
import qualified Irc.Message            as I
import qualified Irc.RawIrcMsg          as I
import qualified Irc.UserInfo           as I
import qualified Kornel.Config          as C
import           Network.HTTP.Client    (Request, requestHeaders)
import           Prelude                hiding (Handler)
import           System.IO
import           System.Random          (randomRIO)

newtype Handler a b =
  Handler (C.Config -> a -> IO (Maybe b, Handler a b))

emptyHandler :: Handler a b
emptyHandler = Handler $ \_ _ -> return (Nothing, emptyHandler)

type LineHandler = Handler I.IrcMsg I.RawIrcMsg

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.UserInfo, I.Identifier, Text) Text

onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg = onlyPrivmsg' I.ircPrivmsg

onlyPrivmsgRespondWithNotice :: PrivmsgHandler -> LineHandler
onlyPrivmsgRespondWithNotice = onlyPrivmsg' I.ircNotice

onlyPrivmsg' :: (Text -> Text -> I.RawIrcMsg) -> PrivmsgHandler -> LineHandler
onlyPrivmsg' how (Handler handler) =
  Handler $ \cfg ->
    \case
      I.Privmsg source target msg -> do
        (response, newHandler) <- handler cfg (source, target, msg)
        let replyTo =
              if isChannelIdentifier target
                then target
                else I.userNick source
        let realResponse = how (I.idText replyTo) <$> response
        return (realResponse, onlyPrivmsg newHandler)
      _ -> return (Nothing, onlyPrivmsg $ Handler handler)

randomElem :: [a] -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem xs = Just . (Unsafe.!!) xs <$> randomRIO (0, length xs - 1)

meAction :: Text -> Text
meAction act = "\001ACTION " <> act <> "\001"

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe p = eitherToMaybe . parseOnly p

discardException :: IO a -> IO (Maybe a)
discardException action =
  E.catch (Just <$> action) $ \(e :: E.SomeException) -> do
    hPutStrLn stderr $ "Error: " ++ show e
    return Nothing

isChannelIdentifier :: I.Identifier -> Bool
isChannelIdentifier ident =
  any @[_] (`isPrefixOf` I.idText ident) ["#", "!", "&"]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe =
  \case
    Left _ -> Nothing
    Right b -> Just b

setupUserAgent :: Request -> Request
setupUserAgent r =
  r
    { requestHeaders =
        [ ("Accept-Language", "en-US,en;q=0.8")
        , ( "Accept"
          , "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
        , ("User-Agent", "kornel +https://github.com/michalrus/kornel")
        ]
    }

decodeHtmlEntities :: Text -> Text
decodeHtmlEntities = TL.toStrict . TLB.toLazyText . htmlEncodedText

decodeUtf8_ :: ByteString -> Text
decodeUtf8_ = decodeUtf8With (\_ _ -> Just '_')
