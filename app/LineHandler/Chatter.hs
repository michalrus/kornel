{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module LineHandler.Chatter
       ( handle
       ) where

import LineHandler
import Data.Text as Text
import qualified IrcParser as I

handle :: LineHandler
handle = onlyPrivmsg handleP

handleP :: PrivmsgHandler
handleP = Handler $ \(origin, _, msg) ->
  if (toUpper $ selfNick) `isInfixOf` (toUpper $ msg) then
    return (Just $ Text.concat [I.nick origin, ": co tam? Zażółć gęślą jaźń! 😼"], handleP)
  else return (Nothing, handleP)
  where
    selfNick = "kornel"        -- FIXME: badly!
