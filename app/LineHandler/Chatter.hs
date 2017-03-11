{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module LineHandler.Chatter
       ( handle
       ) where

import LineHandler
import Data.Text as Text
import qualified IrcParser as I
import qualified CLI as C

handle :: LineHandler
handle = onlyPrivmsg handleP

handleP :: PrivmsgHandler
handleP = Handler $ \cfg (origin, _, msg) ->
  if (toUpper $ C.nick cfg) `isInfixOf` (toUpper $ msg) then
    return (Just $ Text.concat [I.nick origin, ": co tam? Zażółć gęślą jaźń!"], handleP)
  else return (Nothing, handleP)
