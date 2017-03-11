module LineHandler.Chatter
       ( handle
       ) where

import LineHandler
import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text as T
import qualified IrcParser as I
import qualified CLI as C

newtype HState = HState [Text]

handle :: LineHandler
handle = onlyPrivmsg $ handleP $ HState []

handleP :: HState -> PrivmsgHandler
handleP state = Handler $ \cfg (origin, _, msg) ->
  if (toUpper $ C.nick cfg) `isInfixOf` (toUpper $ msg) then do
    let question = fromMaybe msg $ runParser (noHighlight $ C.nick cfg) msg
    let highlight t = (I.nick origin) <> ": " <> t
    (newState, answer) <- chatter state question
    return (highlight <$> answer, handleP newState)
  else return (Nothing, handleP state)

noHighlight :: Text -> Parser Text
noHighlight myNick =
  skipSpace *> asciiCI myNick *> skipSpace *> optional (char ':' <|> char ',')
  *> skip isHorizontalSpace *> takeText

chatter :: HState -> Text -> IO (HState, Maybe Text)
chatter (HState state) msg = do
  let newState = Prelude.take 5 $ msg : state
  -- TODO: continue here
  let answer = "Last 5 messages: “" <> (T.intercalate "”, “" newState) <> "”."
  return (HState newState, Just answer)
