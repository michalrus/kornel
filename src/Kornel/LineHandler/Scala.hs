module Kornel.LineHandler.Scala
  ( handle
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text        as P
import           Data.Semigroup              ((<>))
import           Kornel.CLI                  as C
import           Kornel.LineHandler
import qualified Kornel.LineHandler.BotProxy as Proxy

handle :: LineHandler
handle =
  Proxy.handle C.scalaBotNicks $ do
    skipSpace
    asciiCI "@scala" *> spc
    command <-
      ((asciiCI ":type" <|> asciiCI ":t") *> spc *> pure "!type") <|> pure "!"
    expr <- takeText
    return (command <> " " <> expr)
  where
    spc = void . many1 $ skip isHorizontalSpace
