{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module LineHandler
       ( Handler(..)
       , LineHandler
       , PrivmsgHandler
       , onlyPrivmsg
       )
       where

import qualified IrcParser as I
import Data.Text

data Handler a b = Handler (a -> IO (b, Handler a b))

type LineHandler = Handler I.IrcLine (Maybe I.IrcCommand)

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.Hostmask, Text, Text) (Maybe Text)

onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg (Handler handler) =
  Handler $ \case
    I.IrcLine (Just origin) (I.Privmsg target msg) -> do
      (response, newHandler) <- handler (origin, target, msg)
      let selfNick = "kornel"        -- FIXME: badly!
      let replyTo = if (target /= selfNick) then target else I.nick origin
      let realResponse = I.Privmsg replyTo <$> response
      return (realResponse, onlyPrivmsg newHandler)
    _ -> return (Nothing, onlyPrivmsg $ Handler handler)
