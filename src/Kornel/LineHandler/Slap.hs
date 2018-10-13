module Kornel.LineHandler.Slap
  ( setup
  ) where

import           Data.Attoparsec.Text as P
import           Data.Char            (isSpace)
import qualified Data.List            as Unsafe
import           Data.Text            as T
import           Kornel.Common
import           Kornel.LineHandler
import           Prelude              hiding (Handler, handle)

setup :: HandlerRaw
setup =
  withHelp cmdHelp . onlySimple . pure $ \respond _ request -> do
    let nicks = parseMaybe cmdParser request
    let renderedReasons = reasons <$> nicks
    reason <- join <$> randomElem `traverse` renderedReasons
    mapM_ (respond . Privmsg) reason

cmdParser :: Parser [Text]
cmdParser = skipSpace *> asciiCI "@slap" *> many1 nick
  where
    nick = skipSpace *> takeWhile1 (not . isSpace)

cmdHelp :: Text
cmdHelp = "@slap <nick1>[ <nick2> … ]"

-- | Reasons taken from the wonderful lambdabot. ♥
{-# ANN reasons ("HLint: ignore Redundant $" :: String) #-}

reasons :: [Text] -> [Text]
reasons nicks =
  [ me $ "slaps " ++ x ++ "."
  , me $ "smacks " ++ x ++ " about with a large trout."
  , me $ "beats up " ++ x ++ "."
  , me $ "pokes " ++ x ++ " in the eye."
  , "Why on earth would I slap " ++ x ++ "?"
  , "*SMACK*, *SLAM*, take that, " ++ x ++ "!"
  , me $ "activates her slap-o-matic…"
  , me $ "orders her trained monkeys to punch " ++ x ++ "."
  , me $ "smashes a lamp on " ++ px ++ " head."
  , me $ "hits " ++ x ++ " with a hammer, so they break into a thousand pieces."
  , me $ "throws some pointy lambdas at " ++ x ++ "."
  , me $ "loves " ++ x ++ ", so no slapping."
  , me $ "would never hurt " ++ x ++ "!"
  , "Go slap " ++ x ++ " yourself."
  , "I won’t; I want to go get some cookies instead."
  , "I’d rather not; " ++ x ++ " looks rather dangerous."
  , "I don’t perform such side effects on command!"
  , "Stop telling me what to do."
  , me $ "clobbers " ++ x ++ " with an untyped language."
  , me $ "pulls " ++ x ++ " through the Evil Mangler."
  , me $ "secretly deletes " ++ px ++ " source code."
  , me $ "places her fist firmly on " ++ px ++ " jaw."
  , me $ "locks up " ++ x ++ " in a Monad."
  , me $ "submits " ++ px ++ " email address to a dozen spam lists."
  , me $
    "moulds " ++ x ++ " into a delicious cookie, and places it in her oven."
  , me $ "will count to five…"
  , me $ "jabs " ++ x ++ " with a C pointer."
  , me $ "is overcome by a sudden desire to hurt " ++ x ++ "."
  , me $ "karate-chops " ++ x ++ " into two equally sized halves."
  , "Come on, let's all slap " ++ x ++ "."
  , me $ "pushes " ++ x ++ " from his chair."
  , me $ "hits " ++ x ++ " with an assortment of kitchen utensils."
  , me $ "slaps " ++ x ++ " with a slab of concrete."
  , me $ "puts on her slapping gloves, and slaps " ++ x ++ "."
  , me $
    "decomposes " ++
    x ++
    " into several parts using the Banach-Tarski theorem and reassembles them to get two copies of " ++
    x ++ "!"
  ]
  where
    x = listPeople nicks
    px = listPeople $ possessive <$> nicks
    me = meAction

--8<----------------- TODO: move to Common? ----------------->8--
possessive :: Text -> Text
possessive x
  | T.null x = ""
  | T.last x == 's' = x ++ "’"
  | otherwise = x ++ "’s"

listPeople :: [Text] -> Text
listPeople =
  \case
    [x] -> x
    [x1, x2] -> x1 ++ " and " ++ x2
    xs -> T.intercalate ", " (Unsafe.init xs) ++ ", and " ++ Unsafe.last xs
