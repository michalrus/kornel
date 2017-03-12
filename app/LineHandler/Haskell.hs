module LineHandler.Haskell
       ( handle
       ) where

import LineHandler
import CLI as C
import qualified IrcParser as I
import Control.Applicative
import Control.Monad
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Attoparsec.Text as P

-- FIXME: remove the duplication between Haskell and Scala handlers with a universal BotProxy handler

haskellbotNick :: Text
haskellbotNick = "lambdabot"

data HState = HState
              { lastReplyTo :: Maybe Text
              }

handle :: LineHandler
handle = handle' $ HState Nothing

handle' :: HState -> LineHandler
handle' state = Handler $ \cfg -> \case

  I.IrcLine (Just origin) (I.Privmsg target msg)

    | I.nick origin == haskellbotNick -> do
        let r = (\to -> I.Privmsg to msg) <$> lastReplyTo state
        return (r, handle' state)

    | otherwise ->
        case runParser cmdParser msg of
          Just (command) -> do
            let replyTo = if (target /= C.nick cfg) then target else I.nick origin
            return (Just $ I.Privmsg haskellbotNick $ command,
                    handle' $ state { lastReplyTo = Just replyTo })

          _ -> return (Nothing, handle' state)

  _ -> return (Nothing, handle' state)

cmdParser :: Parser (Text)
cmdParser = do
  skipSpace
  (asciiCI "@haskell" <|> asciiCI "@hs") *> spc
  command
    <-  ((asciiCI ":type"   <|> asciiCI ":t") *> spc *> pure "@type")
    <|> ((asciiCI ":kind"   <|> asciiCI ":k") *> spc *> pure "@kind")
    <|> ((asciiCI ":pl"                     ) *> spc *> pure "@pl")
    <|> ((asciiCI ":pointful"               ) *> spc *> pure "@pointful")
    <|> ((asciiCI ":free"                   ) *> spc *> pure "@free")
    <|> ((asciiCI ":instances"              ) *> spc *> pure "@instances")
    <|> ((asciiCI ":hoogle"                 ) *> spc *> pure "@hoogle")
    <|> ((asciiCI ":index"                  ) *> spc *> pure "@index")
    <|> ((asciiCI ":djinn"                  ) *> spc *> pure "@djinn")
    <|> ((asciiCI ":undo"                   ) *> spc *> pure "@undo")
    <|> ((asciiCI ":unmtl"                  ) *> spc *> pure "@unmtl")
    <|> ((asciiCI ":source" <|> asciiCI ":src" <|> asciiCI ":s") *> spc *> pure "@src")
    <|> (pure ">")
  expr <- takeText
  return (command <> " " <> expr)
  where
    spc = void . many1 $ skip isHorizontalSpace
