module Kornel.LineHandler
  ( HandlerRaw
  , HandlerSimple
  , onlySimple
  , SimpleReply(..)
  , Help(..)
  , helpHandler
  , skipSpace1
  ) where

import qualified Data.Attoparsec.Text as P
import           Data.Char            (isSpace)
import qualified Data.Map             as Map
import qualified Data.Text            as T
import qualified Irc.Commands         as I
import qualified Irc.Identifier       as I
import qualified Irc.Message          as I
import qualified Irc.RawIrcMsg        as I
import qualified Irc.UserInfo         as I
import           Kornel.Common

type HandlerRaw = (I.RawIrcMsg -> IO ()) -> IO (I.IrcMsg -> IO ())

-- |A slightly different structure is needed, because we need to carry
-- a context of where to send the reply back to.
type HandlerSimple = IO ((SimpleReply -> IO ()) -> I.UserInfo -> Text -> IO ())

data SimpleReply
  = Privmsg Text
  | Notice Text
  deriving (Eq, Show)

newtype Help =
  Help (Map [Text] Text)

onlySimple :: HandlerSimple -> HandlerRaw
onlySimple handlerSimple respondRaw = do
  handlerSimple' <- handlerSimple
  pure
    (\case
       I.Privmsg source target msg -> do
         let replyTo =
               if isChannelIdentifier target
                 then target
                 else I.userNick source
         handlerSimple'
           (\response' ->
              let (how, response) =
                    case response' of
                      Privmsg t -> (I.ircPrivmsg, t)
                      Notice t -> (I.ircNotice, t)
               in forM_
                    (filter (not . null . T.strip) . T.split (== '\n') $
                     response)
                    (respondRaw . how (I.idText replyTo)))
           source
           msg
       _ -> pure ())

skipSpace1 :: P.Parser ()
skipSpace1 = P.space *> P.skipSpace

helpHandler :: [Help] -> (Help, HandlerRaw)
helpHandler helps =
  (cmdHelp, ) . onlySimple . pure $ \respond _ msg ->
    forM_ (parseMaybe cmdParser msg) $ \case
      [] -> respond . Notice $ allCommands
      cmds ->
        forM_ cmds $ \cmd -> forM_ (lookup cmd singleCommand) (respond . Notice)
  where
    cmdHelp = Help [(["help"], "<cmd>")]
    cmdParser :: P.Parser [Text]
    cmdParser =
      P.skipSpace *> "@help" *>
      many (skipSpace1 *> optional (P.char '@') *> P.takeWhile1 (not . isSpace)) <*
      P.skipSpace <*
      P.endOfInput
    allCommands :: Text
    allCommands =
      let explicite :: [Text] = do
            Help help <- helps
            (cmds, _) <- Map.toList help
            cmds
          implicite :: [Text] = do
            Help help <- helps
            (cmds, hlp) <- Map.toList help
            [hlp | null cmds]
       in intercalate " • " $
          (sort . map ("@" ++) $ explicite) ++ sort implicite
    singleCommand :: Map Text Text
    singleCommand =
      Map.fromList $ do
        Help help <- helps
        (cmds, hlp) <- Map.toList help
        cmd <- cmds
        pure (cmd, "@" ++ cmd ++ " " ++ hlp)
