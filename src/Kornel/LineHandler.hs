module Kornel.LineHandler
  ( HandlerRaw
  , HandlerSimple
  , onlySimple
  , SimpleReply(..)
  , withHelp
  , skipSpace1
  ) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text            as T
import qualified Irc.Commands         as I
import qualified Irc.Identifier       as I
import qualified Irc.Message          as I
import qualified Irc.RawIrcMsg        as I
import qualified Irc.UserInfo         as I
import           Kornel.Common

type HandlerRaw = (I.RawIrcMsg -> IO ()) -> IO (I.IrcMsg -> IO ())

-- |A slightly different structure is needed, because we need to carry
-- a context of where to send the reply back to.
type HandlerSimple = IO ((SimpleReply -> IO ()) -> I.UserInfo -> Text -> IO ())

data SimpleReply
  = Privmsg Text
  | Notice Text
  deriving (Eq, Show)

onlySimple :: HandlerSimple -> HandlerRaw
onlySimple handlerSimple respondRaw = do
  handlerSimple' <- handlerSimple
  pure
    (\case
       I.Privmsg source target msg -> do
         let replyTo =
               if isChannelIdentifier target
                 then target
                 else I.userNick source
         handlerSimple'
           (\response' ->
              let (how, response) =
                    case response' of
                      Privmsg t -> (I.ircPrivmsg, t)
                      Notice t -> (I.ircNotice, t)
               in forM_
                    (filter (not . null . T.strip) . T.split (== '\n') $
                     response)
                    (respondRaw . how (I.idText replyTo)))
           source
           msg
       _ -> pure ())

skipSpace1 :: P.Parser ()
skipSpace1 = P.space *> P.skipSpace

withHelp :: Text -> HandlerRaw -> HandlerRaw
withHelp txt = merge handlerHelp
  where
    handlerHelp :: HandlerRaw
    handlerHelp =
      onlySimple $
      pure
        (\respond _ ->
           \case
             "@help" -> respond . Notice $ "• " ++ txt
             _ -> pure ())
    merge :: HandlerRaw -> HandlerRaw -> HandlerRaw
    merge a b respond = do
      a' <- a respond
      b' <- b respond
      pure (\msg -> a' msg >> b' msg)
