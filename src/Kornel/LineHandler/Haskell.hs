module Kornel.LineHandler.Haskell
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
handle = Proxy.handle C.haskellBotNicks $ do
  skipSpace
  (asciiCI "@haskell" <|> asciiCI "@hs") *> spc
  command
    <-  ((asciiCI ":type" <|> asciiCI ":t") *> spc *> pure "@type")
    <|> ((asciiCI ":kind" <|> asciiCI ":k") *> spc *> pure "@kind")
    <|> ( asciiCI ":pl"                     *> spc *> pure "@pl")
    <|> ( asciiCI ":pointful"               *> spc *> pure "@pointful")
    <|> ( asciiCI ":free"                   *> spc *> pure "@free")
    <|> ( asciiCI ":instances"              *> spc *> pure "@instances")
    <|> ( asciiCI ":hoogle"                 *> spc *> pure "@hoogle")
    <|> ( asciiCI ":index"                  *> spc *> pure "@index")
    <|> ( asciiCI ":djinn"                  *> spc *> pure "@djinn")
    <|> ( asciiCI ":undo"                   *> spc *> pure "@undo")
    <|> ( asciiCI ":unmtl"                  *> spc *> pure "@unmtl")
    <|> ((asciiCI ":source" <|> asciiCI ":src" <|> asciiCI ":s") *> spc *> pure "@src")
    <|> pure ">"
  expr <- takeText
  return (command <> " " <> expr)
  where
    spc = void . many1 $ skip isHorizontalSpace
