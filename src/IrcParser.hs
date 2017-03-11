module IrcParser
       ( IrcLine(..)
       , IrcCommand(..)
       , Hostmask(..)
       , readMessage
       , showCommand
       ) where

import Control.Applicative
import Data.Text as Text
import Data.Attoparsec.Text as P
import Text.Printf (printf)
import qualified Data.List as L

data Hostmask = Hostmask
                 { nick :: Text
                 , hostmask :: Maybe Text
                 , host :: Maybe Text
                 }
               deriving (Show, Eq)

data IrcLine = IrcLine (Maybe Hostmask) IrcCommand
  deriving (Show, Eq)

data IrcCommand
  = Ping Text
  | Pong Text
  | Nick Text
  | User Text Text Text Text
  | Join [Text]
  | Part [Text] (Maybe Text)
  | Mode Text Text [Text]
  | Notice Text Text
  | Privmsg Text Text
  | StringCommand Text [Text]
  | NumericCommand Integer [Text]
  deriving (Show, Eq)

readMessage :: Text -> Either String IrcLine
readMessage msg = parseOnly parseMessage msg

showCommand :: IrcCommand -> Text
showCommand = \case
  Ping t             -> c ["PING :", t]
  Pong t             -> c ["PONG :", t]
  Nick n             -> c ["NICK ", n]
  User user a b real -> c ["USER ", user, " ", a, " ", b, " :", real]
  Join chs           -> c ["JOIN ", intercalate "," chs]
  Part chs reason    -> c ["PART ", intercalate "," chs, maybe "" (append " :") reason]
  Mode t m args      -> c $ ["MODE ", t, " ", m, " "] ++ L.intersperse " " args
  Notice t m         -> c ["NOTICE ", t, " :", m]
  Privmsg t m        -> c ["PRIVMSG ", t, " :", m]
  StringCommand  name args -> c $ name                        : colonizeArgs args
  NumericCommand name args -> c $ (pack $ printf "%03i" name) : colonizeArgs args
  where
    c = Text.concat
    colonizeArgs xs = if Prelude.null xs then [] else Prelude.init xs ++ [append ":" $ Prelude.last xs]

parseMessage :: Parser IrcLine
parseMessage =
  IrcLine <$> option Nothing (Just <$> (skipMany space *> char ':' *> parseHostmask))
          <*> parseCommand

parseHostmask :: Parser Hostmask
parseHostmask = full <|> nickOnly
  where
    full     = Hostmask <$> (P.takeTill $ \c -> isWhitespace c || c == '!') <* char '!'
                        <*> (Just <$> (P.takeTill $ \c -> isWhitespace c || c == '@')) <* char '@'
                        <*> (Just <$> (P.takeTill isWhitespace))
    nickOnly = Hostmask <$> P.takeTill isWhitespace <*> pure Nothing <*> pure Nothing

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
      cmd <- (takeWhile1 $ inClass "A-Za-z0-9_")
      case cmd of
        "PING"    -> Ping    <$> argument
        "PONG"    -> Pong    <$> argument
        "NICK"    -> Nick    <$> argument
        "USER"    -> User    <$> argument <*> argument <*> argument <*> argument
        "JOIN"    -> Join    <$> splitOn "," <$> argument
        "MODE"    -> Mode    <$> argument <*> argument <*> many argument
        "PART"    -> Part    <$> splitOn "," <$> argument <*> optionalArg
        "NOTICE"  -> Notice  <$> argument <*> argument
        "PRIVMSG" -> Privmsg <$> argument <*> argument
        _         -> StringCommand cmd <$> many argument

isWhitespace :: Char -> Bool
isWhitespace c = isHorizontalSpace c || isEndOfLine c
