{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module LineHandler.Slap
       ( handle
       ) where

import LineHandler
import Control.Monad
import Data.Monoid ((<>))
import Data.Text as T
import Data.Attoparsec.Text as P
import System.Random (randomRIO)

handle :: LineHandler
handle = onlyPrivmsg handleP
  where
    handleP = Handler $ \_ (_, _, msg) -> do
      let nicks = runParser cmdParser msg
      let renderedReasons = reasons <$> nicks
      reason <- join <$> randomElem `traverse` renderedReasons
      return (reason, handleP)

cmdParser :: Parser [Text]
cmdParser = skipSpace *> asciiCI "@slap" *> many1 nick
  where
    nick = skipSpace *> takeWhile1 (not . isHorizontalSpace)

-- | Reasons taken from the wonderful lambdabot. ♥
reasons :: [Text] -> [Text]
reasons nicks =
  [ me $ "slaps " <> x <> "."
  , me $ "smacks " <> x <> " about with a large trout."
  , me $ "beats up " <> x <> "."
  , me $ "pokes " <> x <> " in the eye."
  , "Why on earth would I slap " <> x <> "?"
  , "*SMACK*, *SLAM*, take that, " <> x <> "!"
  , me $ "activates her slap-o-matic…"
  , me $ "orders her trained monkeys to punch " <> x <> "."
  , me $ "smashes a lamp on " <> px <> " head."
  , me $ "hits " <> x <> " with a hammer, so they break into a thousand pieces."
  , me $ "throws some pointy lambdas at " <> x <> "."
  , me $ "loves " <> x <> ", so no slapping."
  , me $ "would never hurt " <> x <> "!"
  , "Go slap " <> x <> " yourself."
  , "I won’t; I want to go get some cookies instead."
  , "I’d rather not; " <> x <> " looks rather dangerous."
  , "I don’t perform such side effects on command!"
  , "Stop telling me what to do."
  , me $ "clobbers " <> x <> " with an untyped language."
  , me $ "pulls " <> x <> " through the Evil Mangler."
  , me $ "secretly deletes " <> px <> " source code."
  , me $ "places her fist firmly on " <> px <> " jaw."
  , me $ "locks up " <> x <> " in a Monad."
  , me $ "submits " <> px <> " email address to a dozen spam lists."
  , me $ "moulds " <> x <> " into a delicious cookie, and places it in her oven."
  , me $ "will count to five…"
  , me $ "jabs " <> x <> " with a C pointer."
  , me $ "is overcome by a sudden desire to hurt " <> x <> "."
  , me $ "karate-chops " <> x <> " into two equally sized halves."
  , "Come on, let's all slap " <> x <> "."
  , me $ "pushes " <> x <> " from his chair."
  , me $ "hits " <> x <> " with an assortment of kitchen utensils."
  , me $ "slaps " <> x <> " with a slab of concrete."
  , me $ "puts on her slapping gloves, and slaps " <> x <> "."
  , me $ "decomposes " <> x <> " into several parts using the Banach-Tarski theorem and reassembles them to get two copies of " <> x <> "!"
  ]
  where
    x = listPeople nicks
    px = listPeople $ possessive <$> nicks
    me = meAction

--8<----------------- TODO: move to LineHandler? ----------------->8--

possessive :: Text -> Text
possessive x
 | T.last x == 's' = x <> "’"
 | otherwise       = x <> "’s"

listPeople :: [Text] -> Text
listPeople = \case
  x1      : [] -> x1
  x1 : x2 : [] -> x1 <> " and " <> x2
  xs           -> (T.intercalate ", " $ Prelude.init xs) <> " and " <> Prelude.last xs

randomElem :: [a] -> IO (Maybe a)
randomElem [] = pure Nothing
randomElem xs = Just . (!!) xs <$> randomRIO (0, Prelude.length xs - 1)

meAction :: Text -> Text
meAction act = "\001ACTION " <> act <> "\001"

runParser :: Parser a -> Text -> Maybe a
runParser p t = discardError $ parseOnly p t
  where discardError = \case
          Left _ -> Nothing
          Right b -> Just b
