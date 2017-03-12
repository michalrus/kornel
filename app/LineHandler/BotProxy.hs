module LineHandler.BotProxy
       ( handle
       ) where

import LineHandler
import CLI as C
import qualified IrcParser as I
import Data.Text as T
import Data.Attoparsec.Text as P

data HState = HState
              { lastReplyTo :: Maybe Text
              }

handle :: Text -> Parser Text -> LineHandler
handle botNick commandParser =
  handle' $ HState Nothing
  where
    handle' :: HState -> LineHandler
    handle' state = Handler $ \cfg -> \case

      I.IrcLine (Just origin) (I.Privmsg target msg)

        | I.nick origin == botNick -> do
            let r = (\to -> I.Privmsg to msg) <$> lastReplyTo state
            return (r, handle' state)

        | otherwise ->
            case runParser commandParser msg of
              Just (command) -> do
                let replyTo = if (target /= C.nick cfg) then target else I.nick origin
                return (Just $ I.Privmsg botNick $ command,
                        handle' $ state { lastReplyTo = Just replyTo })

              _ -> return (Nothing, handle' state)

      _ -> return (Nothing, handle' state)
