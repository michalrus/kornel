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
import qualified CLI as C

data Handler a b = Handler (C.Config -> a -> IO (b, Handler a b))

type LineHandler = Handler I.IrcLine (Maybe I.IrcCommand)

-- | If you only want to react with text in response to text messages, use this.
type PrivmsgHandler = Handler (I.Hostmask, Text, Text) (Maybe Text)

onlyPrivmsg :: PrivmsgHandler -> LineHandler
onlyPrivmsg (Handler handler) =
  Handler $ \cfg -> \case
    I.IrcLine (Just origin) (I.Privmsg target msg) -> do
      (response, newHandler) <- handler cfg (origin, target, msg)
      let replyTo = if (target /= C.nick cfg) then target else I.nick origin
      let realResponse = I.Privmsg replyTo <$> response
      return (realResponse, onlyPrivmsg newHandler)
    _ -> return (Nothing, onlyPrivmsg $ Handler handler)
