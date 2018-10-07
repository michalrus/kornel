module Kornel.LineHandler.Scala
  ( handle
  ) where

import           Data.Attoparsec.Text        as P
import           Kornel.Config               as C
import           Kornel.LineHandler
import qualified Kornel.LineHandler.BotProxy as Proxy
import           Prelude                     hiding (Handler, handle)

handle :: LineHandler
handle =
  Proxy.handle C.scalaBotNicks $ do
    skipSpace
    asciiCI "@scala" *> spc
    command <-
      ((asciiCI ":type" <|> asciiCI ":t") *> spc $> "!type") <|> pure "!"
    expr <- takeText
    return (command <> " " <> expr)
  where
    spc = void . many1 $ skip isHorizontalSpace
