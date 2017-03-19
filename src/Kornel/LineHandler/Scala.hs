module Kornel.LineHandler.Scala
       ( handle
       ) where

import Kornel.LineHandler
import Kornel.CLI as C
import Control.Applicative
import Control.Monad
import Data.Semigroup ((<>))
import Data.Attoparsec.Text as P
import qualified Kornel.LineHandler.BotProxy as Proxy

handle :: LineHandler
handle = Proxy.handle C.scalaBotNicks $ do
  skipSpace
  asciiCI "@scala" *> spc
  command
    <-  ((asciiCI ":type" <|> asciiCI ":t") *> spc *> pure "!type")
    <|> pure "!"
  expr <- takeText
  return (command <> " " <> expr)
  where
    spc = void . many1 $ skip isHorizontalSpace
