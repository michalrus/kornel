module Kornel.Common where

import           Data.Attoparsec.Text   as P
import           Data.ByteString        (ByteString)
import qualified Data.List              as Unsafe
import           Data.Text.Encoding     (decodeUtf8With)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           HTMLEntities.Decoder   (htmlEncodedText)
import qualified Irc.Identifier         as I
import qualified Kornel.Log             as L
import           Network.HTTP.Client    (Request, requestHeaders)
import           Prelude                hiding (Handler)
import           System.Random          (randomRIO)

asyncWithLog :: Text -> IO () -> IO ()
asyncWithLog name =
  void . async . handleAny (L.log . (("-ERROR- " ++ name ++ ": ") ++) . tshow)

randomElem :: [a] -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem xs = Just . (Unsafe.!!) xs <$> randomRIO (0, length xs - 1)

meAction :: Text -> Text
meAction act = "\001ACTION " <> act <> "\001"

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe p = eitherToMaybe . parseOnly p

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
