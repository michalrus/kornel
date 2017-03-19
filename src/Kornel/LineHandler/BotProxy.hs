module Kornel.LineHandler.BotProxy
       ( handle
       , nextElem
       ) where

import Kornel.LineHandler
import Kornel.CLI as C
import qualified IrcParser as I
import Control.Monad
import Control.Monad.Zip
import Data.Maybe (listToMaybe)
import Data.Text as T
import Data.Attoparsec.Text as P

data HState = HState
              { lastReplyTo :: Maybe I.Target
              , lastBotNick :: Maybe I.Target
              , lastBotInquiry :: Maybe Text
              } deriving (Show)

handle :: (Config -> [I.Target]) -> Parser Text -> LineHandler
handle botNicks commandParser =
  handle' $ HState Nothing Nothing Nothing
  where
    handle' :: HState -> LineHandler
    handle' state = Handler $ \cfg -> \case

      I.IrcLine (Just origin) (I.Privmsg target msg)

        | elem (I.nick origin) (botNicks cfg) -> do
            let r = (`I.Privmsg` msg) <$> lastReplyTo state
            return (r, handle' state)

        | otherwise ->
            case runParser commandParser msg of
              Just command -> do
                let replyTo = if I.isChannel target then target else I.nick origin
                let bot = listToMaybe $ botNicks cfg
                return ((`I.Privmsg` command) <$> bot,
                        handle' $ state { lastReplyTo = Just replyTo
                                        , lastBotNick = bot
                                        , lastBotInquiry = Just command
                                        })

              _ -> return (Nothing, handle' state)

      I.IrcLine _ (I.NumericCommand 401 (_ : target : _))
        | (Just . I.Target) target == lastBotNick state -> do
            let nextNick = nextElem (botNicks cfg) $ I.Target target
            case nextNick `mzip` lastBotInquiry state of
              Just (to, command) ->
                return (Just $ I.Privmsg to command,
                        handle' state { lastBotNick = Just to
                                      })
              Nothing ->
                return (Nothing,
                        handle' state { lastReplyTo = Nothing
                                      , lastBotNick = Nothing
                                      , lastBotInquiry = Nothing
                                      })

      _ -> return (Nothing, handle' state)

nextElem :: Eq a => [a] -> a -> Maybe a
nextElem xs after
  | after `notElem` xs = listToMaybe xs
  | otherwise
    = listToMaybe
    . mfilter (/= after)
    . Prelude.dropWhile (/= after)
    $ xs
