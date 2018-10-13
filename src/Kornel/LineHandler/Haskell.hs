module Kornel.LineHandler.Haskell
  ( setup
  ) where

import           Data.Attoparsec.Text        as P
import           Kornel.Config               as C
import           Kornel.LineHandler
import qualified Kornel.LineHandler.BotProxy as Proxy
import           Prelude                     hiding (Handler, handle)

setup :: Config -> HandlerRaw
setup cfg =
  withHelp cmdHelp . Proxy.setup (C.haskellBotNicks cfg) $ do
    skipSpace
    (asciiCI "@haskell" <|> asciiCI "@hs") *> spc
    command <-
      ((asciiCI ":type" <|> asciiCI ":t") *> spc $> "@type") <|>
      ((asciiCI ":kind" <|> asciiCI ":k") *> spc $> "@kind") <|>
      (asciiCI ":pl" *> spc $> "@pl") <|>
      (asciiCI ":pointful" *> spc $> "@pointful") <|>
      (asciiCI ":free" *> spc $> "@free") <|>
      (asciiCI ":instances" *> spc $> "@instances") <|>
      (asciiCI ":hoogle" *> spc $> "@hoogle") <|>
      (asciiCI ":index" *> spc $> "@index") <|>
      (asciiCI ":djinn" *> spc $> "@djinn") <|>
      (asciiCI ":undo" *> spc $> "@undo") <|>
      (asciiCI ":unmtl" *> spc $> "@unmtl") <|>
      ((asciiCI ":source" <|> asciiCI ":src" <|> asciiCI ":s") *> spc $> "@src") <|>
      pure ">"
    expr <- takeText
    return (command ++ " " ++ expr)
  where
    spc = skipSpace

cmdHelp :: Text
cmdHelp =
  "{ @haskell | @hs } [ :type | :t | :kind | :k | :pl | :pointful | :free | :instances | :hoogle | :index | :djinn | :undo | :unmtl | :source | :src | :s ] <expr>"
