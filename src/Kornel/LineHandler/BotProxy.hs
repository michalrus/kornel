module Kornel.LineHandler.BotProxy
  ( handle
  , nextElem
  ) where

import           Control.Monad
import           Control.Monad.Zip
import           Data.Attoparsec.Text as P
import qualified Irc.Codes            as I
import qualified Irc.Commands         as I
import qualified Irc.Identifier       as I
import qualified Irc.Message          as I
import qualified Irc.UserInfo         as I
import           Kornel.Config        as C
import           Kornel.LineHandler
import           Prelude              hiding (Handler, handle)

data HState = HState
  { lastReplyTo :: Maybe I.Identifier
  , lastBotNick :: Maybe I.Identifier
  , lastBotInquiry :: Maybe Text
  } deriving (Show)

handle :: (Config -> [I.Identifier]) -> Parser Text -> LineHandler
handle botNicks commandParser = handle' $ HState Nothing Nothing Nothing
  where
    handle' :: HState -> LineHandler
    handle' state =
      Handler $ \cfg ->
        \case
          I.Privmsg origin target msg
            | elem (I.userNick origin) (botNicks cfg) -> do
              let r = flip I.ircPrivmsg msg . I.idText <$> lastReplyTo state
              return (r, handle' state)
            | otherwise ->
              case parseMaybe commandParser msg of
                Just command -> do
                  let replyTo =
                        if isChannelIdentifier target
                          then target
                          else I.userNick origin
                  let bot = headMay $ botNicks cfg
                  return
                    ( flip I.ircPrivmsg command . I.idText <$> bot
                    , handle' $
                      state
                        { lastReplyTo = Just replyTo
                        , lastBotNick = bot
                        , lastBotInquiry = Just command
                        })
                _ -> return (Nothing, handle' state)
          I.Reply I.ERR_NOSUCHNICK (_:target:_)
            | (Just . I.mkId) target == lastBotNick state -> do
              let nextNick = nextElem (botNicks cfg) $ I.mkId target
              case nextNick `mzip` lastBotInquiry state of
                Just (to, command) ->
                  return
                    ( Just $ I.ircPrivmsg (I.idText to) command
                    , handle' state {lastBotNick = Just to})
                Nothing ->
                  return
                    ( Nothing
                    , handle'
                        state
                          { lastReplyTo = Nothing
                          , lastBotNick = Nothing
                          , lastBotInquiry = Nothing
                          })
          _ -> return (Nothing, handle' state)

nextElem :: Eq a => [a] -> a -> Maybe a
nextElem xs after
  | after `notElem` xs = headMay xs
  | otherwise = headMay . mfilter (/= after) . Prelude.dropWhile (/= after) $ xs
