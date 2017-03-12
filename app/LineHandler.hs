module LineHandler
       ( Handler(..)
       , emptyHandler
       , LineHandler
       , PrivmsgHandler
       , onlyPrivmsg
       , randomElem
       , meAction
       , runParser
       , discardError
       )
       where

import qualified IrcParser as I
import Data.Attoparsec.Text as P
import Data.Monoid ((<>))
import Data.Text
import System.Random (randomRIO)
import qualified CLI as C

data Handler a b = Handler (C.Config -> a -> IO (Maybe b, Handler a b))

emptyHandler :: Handler a b
emptyHandler = Handler $ (\_ _ -> return (Nothing, emptyHandler))

type LineHandler = Handler I.IrcLine I.IrcCommand

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.Hostmask, Text, Text) Text

onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg (Handler handler) =
  Handler $ \cfg -> \case
    I.IrcLine (Just origin) (I.Privmsg target msg) -> do
      (response, newHandler) <- handler cfg (origin, target, msg)
      let replyTo = if (target /= C.nick cfg) then target else I.nick origin
      let realResponse = I.Privmsg replyTo <$> response
      return (realResponse, onlyPrivmsg newHandler)
    _ -> return (Nothing, onlyPrivmsg $ Handler handler)

randomElem :: [a] -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem xs = Just . (!!) xs <$> randomRIO (0, Prelude.length xs - 1)

meAction :: Text -> Text
meAction act = "\001ACTION " <> act <> "\001"

runParser :: Parser a -> Text -> Maybe a
runParser p t = discardError $ parseOnly p t

discardError :: Either a b -> Maybe b
discardError = \case
  Left  _ -> Nothing
  Right b -> Just b
