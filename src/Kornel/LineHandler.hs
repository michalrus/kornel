module Kornel.LineHandler
  ( HandlerRaw
  , HandlerPrivmsg
  , onlyPrivmsg
  , onlyPrivmsgRespondWithNotice
  ) where

import qualified Data.Text      as T
import qualified Irc.Commands   as I
import qualified Irc.Identifier as I
import qualified Irc.Message    as I
import qualified Irc.RawIrcMsg  as I
import qualified Irc.UserInfo   as I
import           Kornel.Common

type HandlerRaw = (I.RawIrcMsg -> IO ()) -> IO (I.IrcMsg -> IO ())

-- |A slightly different structure is needed, because we need to carry
-- a context of where to send the reply back to.
type HandlerPrivmsg = IO ((Text -> IO ()) -> I.UserInfo -> Text -> IO ())

onlyPrivmsg :: HandlerPrivmsg -> HandlerRaw
onlyPrivmsg = onlyPrivmsg' I.ircPrivmsg

onlyPrivmsgRespondWithNotice :: HandlerPrivmsg -> HandlerRaw
onlyPrivmsgRespondWithNotice = onlyPrivmsg' I.ircNotice

onlyPrivmsg' :: (Text -> Text -> I.RawIrcMsg) -> HandlerPrivmsg -> HandlerRaw
onlyPrivmsg' how handlerPrivmsg respondRaw = do
  handlerPrivmsg' <- handlerPrivmsg
  pure
    (\case
       I.Privmsg source target msg -> do
         let replyTo =
               if isChannelIdentifier target
                 then target
                 else I.userNick source
         handlerPrivmsg'
           (\response ->
              forM_
                (filter (not . null . T.strip) . T.split (== '\n') $ response)
                (respondRaw . how (I.idText replyTo)))
           source
           msg
       _ -> pure ())
