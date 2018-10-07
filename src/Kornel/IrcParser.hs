module Kornel.IrcParser
  ( IrcLine(..)
  , IrcCommand(..)
  , Origin(..)
  , Username(..)
  , Realname(..)
  , Hostname(..)
  , Target(..)
  , isChannel
  , readMessage
  , showCommand
  ) where

import           Data.Attoparsec.Text as P
import           Data.Coerce
import qualified Data.List            as Unsafe
import qualified Data.Text            as T
import           Text.Printf          (printf)

newtype Target =
  Target Text
  deriving (Show, Eq, Generic)

newtype Username =
  Username Text
  deriving (Show, Eq, Generic)

newtype Realname =
  Realname Text
  deriving (Show, Eq, Generic)

newtype Hostname =
  Hostname Text
  deriving (Show, Eq, Generic)

isChannel :: Target -> Bool
isChannel (Target s) = any @[_] (`isPrefixOf` s) ["#", "!", "&"]

data Origin = Origin
  { nick :: Target
  , user :: Maybe Username
  , host :: Maybe Hostname
  } deriving (Show, Eq)

data IrcLine =
  IrcLine (Maybe Origin)
          IrcCommand
  deriving (Show, Eq)

data IrcCommand
  = Ping Text
  | Pong Text
  | Nick Target
  | User Username
         Realname
  | Join [Target]
  | Part [Target]
         (Maybe Text)
  | Mode Target
         Text
         [Text]
  | Notice Target
           Text
  | Privmsg Target
            Text
  | StringCommand Text
                  [Text]
  | NumericCommand Integer
                   [Text]
  deriving (Show, Eq)

readMessage :: Text -> Either String IrcLine
readMessage = parseOnly parseMessage

showCommand :: IrcCommand -> Text
showCommand =
  sanitize <$> \case
    Ping t -> "PING :" ++ t
    Pong t -> "PONG :" ++ t
    Nick n -> "NICK " ++ coerce n
    User u r -> "USER " ++ coerce u ++ " - - :" ++ coerce r
    Join chs -> "JOIN " ++ intercalate "," (coerce <$> chs)
    Part chs reason ->
      "PART " ++
      intercalate "," (coerce <$> chs) ++ maybe "" (T.append " :") reason
    Mode t m args -> "MODE " ++ coerce t ++ " " ++ m ++ " " ++ T.unwords args
    Notice t m -> "NOTICE " ++ coerce t ++ " :" ++ m
    Privmsg t m -> "PRIVMSG " ++ coerce t ++ " :" ++ m
    StringCommand name args -> name ++ " " ++ T.unwords (colonize args)
    NumericCommand name args ->
      pack (printf "%03i" name) ++ " " ++ T.unwords (colonize args)
  where
    sanitize :: Text -> Text
    sanitize =
      T.replace "\n" "\\LF" . T.replace "\r" "\\CR" . T.replace "\0" "\\NUL"
    colonize [] = []
    colonize xs = Unsafe.init xs ++ [":" ++ Unsafe.last xs]

parseMessage :: Parser IrcLine
parseMessage =
  IrcLine <$>
  option Nothing (Just <$> (skipMany space *> char ':' *> parseOrigin)) <*>
  parseCommand

parseOrigin :: Parser Origin
parseOrigin = full <|> nickOnly
  where
    full =
      Origin <$> (Target <$> P.takeTill (\c -> isWhitespace c || c == '!')) <*
      char '!' <*>
      (Just . Username <$> P.takeTill (\c -> isWhitespace c || c == '@')) <*
      char '@' <*>
      (Just . Hostname <$> P.takeTill isWhitespace)
    nickOnly =
      Origin <$> (Target <$> P.takeTill isWhitespace) <*> pure Nothing <*>
      pure Nothing

parseCommand :: Parser IrcCommand
parseCommand = skipMany space *> (numericCmd <|> stringCmd)
  where
    argument :: Parser Text
    argument = skipMany1 space *> (lastOne <|> takeWhile1 (not . isWhitespace))
      where
        lastOne = char ':' *> P.takeTill isEndOfLine
    optionalArg :: Parser (Maybe Text)
    optionalArg = option Nothing (Just <$> argument)
    numericCmd :: Parser IrcCommand
    numericCmd = NumericCommand <$> decimal <*> many argument
    stringCmd :: Parser IrcCommand
    stringCmd = do
      cmd <- T.toUpper <$> takeWhile1 (inClass "A-Za-z0-9_")
      case cmd of
        "PING" -> Ping <$> argument
        "PONG" -> Pong <$> argument
        "NICK" -> Nick . Target <$> argument
        "USER" ->
          User . Username <$> argument <*>
          (Realname <$> (argument *> argument *> argument))
        "JOIN" -> Join . map Target . T.splitOn "," <$> argument
        "MODE" -> Mode . Target <$> argument <*> argument <*> many argument
        "PART" -> Part . map Target . T.splitOn "," <$> argument <*> optionalArg
        "NOTICE" -> Notice . Target <$> argument <*> argument
        "PRIVMSG" -> Privmsg . Target <$> argument <*> argument
        _ -> StringCommand cmd <$> many argument

isWhitespace :: Char -> Bool
isWhitespace c = isHorizontalSpace c || isEndOfLine c
