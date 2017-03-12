module LineHandler.Scala
       ( handle
       ) where

import LineHandler
import CLI as C
import qualified IrcParser as I
import Data.Maybe (isNothing)
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

    | otherwise -> do
        let expr = runParser cmdParser msg
        let replyTo = if (target /= C.nick cfg) then target else I.nick origin
        let nextState = if isNothing expr then state else state { lastReplyTo = Just replyTo }
        let toBot e = I.Privmsg scalabotNick ("! " <> e)
        return (toBot <$> expr, handle' nextState)

  _ -> return (Nothing, handle' state)

cmdParser :: Parser Text
cmdParser = skipSpace *> (asciiCI "@scala") *> skip isHorizontalSpace *> takeText
