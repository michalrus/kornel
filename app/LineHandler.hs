module LineHandler
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
       )
       where

import qualified Control.Exception.Base as E
import Data.Attoparsec.Text as P
import Data.Profunctor
import Data.Monoid ((<>))
import Data.Text
import System.IO
import System.Random (randomRIO)
import qualified CLI as C
import qualified IrcParser as I

data Handler a b = Handler (C.Config -> a -> IO (Maybe b, Handler a b))

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
emptyHandler = Handler $ (\_ _ -> return (Nothing, emptyHandler))

type LineHandler = Handler I.IrcLine I.IrcCommand

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.Hostmask, Text, Text) Text

-- FIXME: how to use Profunctor’s dimap to implement this?
onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg (Handler handler) =
  Handler $ \cfg -> \case
    I.IrcLine (Just origin) (I.Privmsg target msg) -> do
      (response, newHandler) <- handler cfg (origin, target, msg)
      let replyTo = if (target /= C.nick cfg) then target else I.nick origin
      let realResponse = I.Privmsg replyTo <$> response
      return (realResponse, onlyPrivmsg newHandler)
    _ -> return (Nothing, onlyPrivmsg $ Handler handler)

onlyPrivmsgRespondWithNotice :: PrivmsgHandler -> LineHandler
onlyPrivmsgRespondWithNotice privmsg =
  dimap id g $ onlyPrivmsg privmsg
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
discardError = \case
  Left  _ -> Nothing
  Right b -> Just b
