module LineHandler.Scala
       ( handle
       ) where

import LineHandler
import CLI as C
import qualified IrcParser as I
import Control.Applicative
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Attoparsec.Text as P

scalabotNick :: Text
scalabotNick = "multibot_"

data HState = HState
              { lastReplyTo :: Maybe Text
              }

handle :: LineHandler
handle = handle' $ HState Nothing

handle' :: HState -> LineHandler
handle' state = Handler $ \cfg -> \case

  I.IrcLine (Just origin) (I.Privmsg target msg)

    | I.nick origin == scalabotNick -> do
        let r = (\to -> I.Privmsg to msg) <$> lastReplyTo state
        return (r, handle' state)

    | otherwise ->
        case runParser cmdParser msg of
          Just (command, expr) -> do
            let replyTo = if (target /= C.nick cfg) then target else I.nick origin
            return (Just $ I.Privmsg scalabotNick $ command <> " " <> expr,
                    handle' $ state { lastReplyTo = Just replyTo })

          _ -> return (Nothing, handle' state)

  _ -> return (Nothing, handle' state)

cmdParser :: Parser (Text, Text)
cmdParser = do
  skipSpace
  asciiCI "@scala" *> spc
  command
    <-  ((asciiCI ":type"  <|> asciiCI ":t") *> spc *> pure "!type")
    <|> (pure "!")
  expr <- takeText
  return (command, expr)
  where
    spc = skip isHorizontalSpace
