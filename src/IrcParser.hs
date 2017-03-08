{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IrcParser
    ( IrcMessage(..)
    , IrcCommand(..)
    , readMessage
    , showCommand
    ) where

import Control.Applicative
import Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 as P
import Data.Char (toUpper)
import Text.Printf

newtype IrcOrigin = IrcOrigin ByteString deriving (Show, Eq)

data IrcMessage = IrcMessage (Maybe IrcOrigin) IrcCommand
  deriving (Show, Eq)

data IrcCommand
  = PingCommand ByteString
  | PongCommand ByteString
  | NickCommand ByteString
  | UserCommand ByteString ByteString ByteString ByteString
  | JoinCommand [ByteString]
  | NoticeCommand ByteString ByteString
  | PrivmsgCommand ByteString ByteString
  | StringCommand ByteString [ByteString]
  | NumericCommand Integer [ByteString]
  deriving (Show, Eq)

readMessage :: ByteString -> Either String IrcMessage
readMessage msg = parseOnly parseMessage msg

showCommand :: IrcCommand -> ByteString
showCommand = \case
  PingCommand t -> c ["PING :", t]
  PongCommand t -> c ["PONG :", t]
  NickCommand n -> c ["NICK ", n]
  UserCommand user a b real -> c ["USER ", user, " ", a, " ", b, " :", real]
  JoinCommand chs -> c ["JOIN ", intercalate "," chs]
  NoticeCommand t m -> c ["NOTICE ", t, " :", m]
  PrivmsgCommand t m -> c ["PRIVMSG ", t, " :", m]
  StringCommand  name args -> c $ name                          : colonizeArgs args
  NumericCommand name args -> c $ (B.pack $ printf "%03i" name) : colonizeArgs args
  where
    c = B.concat
    colonizeArgs xs = if Prelude.null xs then [] else Prelude.init xs ++ [append ":" $ Prelude.last xs]

parseMessage :: Parser IrcMessage
parseMessage =
  IrcMessage <$> option Nothing (Just <$> parseOrigin) <*> parseCommand
  where
    parseOrigin = skipMany space *> char ':' *> (IrcOrigin <$> (P.takeWhile $ not . isWhitespace))

parseCommand :: Parser IrcCommand
parseCommand =
  skipMany space *> (numericCmd <|> stringCmd)
  where
    argument :: Parser ByteString
    argument = skipMany1 space *> (lastOne <|> (takeWhile1 $ not . isWhitespace))
      where lastOne = char ':' *> (P.takeWhile $ not . isEOL)

    numericCmd :: Parser IrcCommand
    numericCmd = NumericCommand <$> decimal <*> many argument

    stringCmd :: Parser IrcCommand
    stringCmd = do
      cmd <- B.map toUpper <$> (takeWhile1 $ inClass "A-Za-z0-9_")
      case cmd of
        "PING" -> PingCommand <$> argument
        "PONG" -> PongCommand <$> argument
        "NICK" -> NickCommand <$> argument
        "USER" -> UserCommand <$> argument <*> argument <*> argument <*> argument
        "JOIN" -> JoinCommand <$> split ',' <$> argument
        "NOTICE" -> NoticeCommand <$> argument <*> argument
        "PRIVMSG" -> PrivmsgCommand <$> argument <*> argument
        _ -> StringCommand cmd <$> many argument

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c || isEOL c || c == '\t'

isEOL :: Char -> Bool
isEOL c = c == '\n' || c == '\r'
