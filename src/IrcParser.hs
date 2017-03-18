
module IrcParser
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

import Control.Applicative
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Attoparsec.Text as P
import Text.Printf (printf)

newtype Target   = Target   { toText :: Text } deriving (Show, Eq)
newtype Username = Username { toText :: Text } deriving (Show, Eq)
newtype Realname = Realname { toText :: Text } deriving (Show, Eq)
newtype Hostname = Hostname { toText :: Text } deriving (Show, Eq)

isChannel :: Target -> Bool
isChannel (Target s) = Prelude.any (flip isPrefixOf s) ["#", "!", "&"]

data Origin = Origin
              { nick :: Target
              , user :: Maybe Username
              , host :: Maybe Hostname
              }
            deriving (Show, Eq)

data IrcLine = IrcLine (Maybe Origin) IrcCommand
  deriving (Show, Eq)

data IrcCommand
  = Ping Text
  | Pong Text
  | Nick Target
  | User Username Realname
  | Join [Target]
  | Part [Target] (Maybe Text)
  | Mode Target Text [Text]
  | Notice Target Text
  | Privmsg Target Text
  | StringCommand Text [Text]
  | NumericCommand Integer [Text]
  deriving (Show, Eq)

readMessage :: Text -> Either String IrcLine
readMessage msg = parseOnly parseMessage msg

showCommand :: IrcCommand -> Text
showCommand = sanitize <$> \case
  Ping t             -> "PING :" <> t
  Pong t             -> "PONG :" <> t
  Nick (Target n)    -> "NICK " <> n
  User
    (Username u)
    (Realname r)     -> "USER " <> u <> " - - :" <> r
  Join chs           -> "JOIN " <> intercalate "," ((toText :: Target -> Text) <$> chs)
  Part chs reason    -> "PART " <> intercalate "," ((toText :: Target -> Text) <$> chs)
                                <> maybe "" (append " :") reason
  Mode
    (Target t) m
    args             -> "MODE " <> t <> " " <> m <> " " <> intercalate " " args
  Notice
    (Target t) m     -> "NOTICE " <> t <> " :" <> m
  Privmsg
    (Target t) m     -> "PRIVMSG " <> t <> " :" <> m
  StringCommand
    name args        -> name                        <> " " <> (intercalate " " $ colonize args)
  NumericCommand
    name args        -> (pack $ printf "%03i" name) <> " " <> (intercalate " " $ colonize args)
  where
    sanitize :: Text -> Text
    sanitize input =
        replace "\n" "\\LF"
      $ replace "\r" "\\CR"
      $ input
    colonize [] = []
    colonize xs = Prelude.init xs ++ [":" <> Prelude.last xs]

parseMessage :: Parser IrcLine
parseMessage =
  IrcLine <$> option Nothing (Just <$> (skipMany space *> char ':' *> parseOrigin))
          <*> parseCommand

parseOrigin :: Parser Origin
parseOrigin = full <|> nickOnly
  where
    full     = Origin <$> (Target <$> (P.takeTill $ \c -> isWhitespace c || c == '!')) <* char '!'
                      <*> (Just . Username <$> (P.takeTill $ \c -> isWhitespace c || c == '@')) <* char '@'
                      <*> (Just . Hostname <$> (P.takeTill isWhitespace))
    nickOnly = Origin <$> (Target <$> P.takeTill isWhitespace) <*> pure Nothing <*> pure Nothing

parseCommand :: Parser IrcCommand
parseCommand =
  skipMany space *> (numericCmd <|> stringCmd)
  where
    argument :: Parser Text
    argument = skipMany1 space *> (lastOne <|> (takeWhile1 $ not . isWhitespace))
      where lastOne = char ':' *> (P.takeTill isEndOfLine)

    optionalArg :: Parser (Maybe Text)
    optionalArg = option Nothing (Just <$> argument)

    numericCmd :: Parser IrcCommand
    numericCmd = NumericCommand <$> decimal <*> many argument

    stringCmd :: Parser IrcCommand
    stringCmd = do
      cmd <- T.toUpper <$> (takeWhile1 $ inClass "A-Za-z0-9_")
      case cmd of
        "PING"    -> Ping    <$> argument
        "PONG"    -> Pong    <$> argument
        "NICK"    -> Nick    <$> Target <$> argument
        "USER"    -> User    <$> Username <$> argument <*> (Realname <$> (argument *> argument *> argument))
        "JOIN"    -> Join    <$> fmap Target . splitOn "," <$> argument
        "MODE"    -> Mode    <$> Target <$> argument <*> argument <*> many argument
        "PART"    -> Part    <$> fmap Target . splitOn "," <$> argument <*> optionalArg
        "NOTICE"  -> Notice  <$> Target <$> argument <*> argument
        "PRIVMSG" -> Privmsg <$> Target <$> argument <*> argument
        _         -> StringCommand cmd <$> many argument

isWhitespace :: Char -> Bool
isWhitespace c = isHorizontalSpace c || isEndOfLine c
