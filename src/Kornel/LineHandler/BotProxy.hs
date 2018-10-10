module Kornel.LineHandler.BotProxy
  ( setup
  , nextElem
  ) where

import           Control.Monad        hiding (forM_)
import           Control.Monad.Zip
import           Data.Attoparsec.Text as P
import qualified Irc.Codes            as I
import qualified Irc.Commands         as I
import qualified Irc.Identifier       as I
import qualified Irc.Message          as I
import qualified Irc.UserInfo         as I
import           Kornel.Common
import           Kornel.LineHandler
import           Prelude              hiding (Handler, handle)

data HState = HState
  { lastReplyTo :: Maybe I.Identifier
  , lastBotNick :: Maybe I.Identifier
  , lastBotInquiry :: Maybe Text
  } deriving (Show)

setup :: [I.Identifier] -> Parser Text -> HandlerRaw
setup botNicks commandParser sendMsg = do
  state <- newTVarIO $ HState Nothing Nothing Nothing
  pure $ \case
    I.Privmsg origin target msg
      | elem (I.userNick origin) botNicks && not (isChannelIdentifier target) -> do
        replyTo <- lastReplyTo <$> readTVarIO state
        forM_ replyTo $ \to -> sendMsg $ I.ircPrivmsg (I.idText to) msg
      | otherwise ->
        case parseMaybe commandParser msg of
          Just command -> do
            let replyTo =
                  if isChannelIdentifier target
                    then target
                    else I.userNick origin
            let bot = headMay botNicks
            atomically $
              modifyTVar'
                state
                (\s ->
                   s
                     { lastReplyTo = Just replyTo
                     , lastBotNick = bot
                     , lastBotInquiry = Just command
                     })
            forM_ bot $ \b -> sendMsg $ I.ircPrivmsg (I.idText b) command
          _ -> pure ()
    I.Reply I.ERR_NOSUCHNICK (_:target:_) -> do
      prevState <- readTVarIO state
      when ((Just . I.mkId) target == lastBotNick prevState) $ do
        let nextNick = nextElem botNicks $ I.mkId target
        case nextNick `mzip` lastBotInquiry prevState of
          Just (to, command) -> do
            atomically $ modifyTVar' state (\s -> s {lastBotNick = Just to})
            sendMsg $ I.ircPrivmsg (I.idText to) command
          Nothing ->
            atomically $
            modifyTVar'
              state
              (\s ->
                 s
                   { lastReplyTo = Nothing
                   , lastBotNick = Nothing
                   , lastBotInquiry = Nothing
                   })
    _ -> pure ()

nextElem :: Eq a => [a] -> a -> Maybe a
nextElem xs after
  | after `notElem` xs = headMay xs
  | otherwise = headMay . mfilter (/= after) . Prelude.dropWhile (/= after) $ xs
