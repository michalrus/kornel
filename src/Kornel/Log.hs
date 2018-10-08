module Kornel.Log
  ( log
  ) where

import qualified Data.Text             as T
import           GHC.Conc.Sync         (myThreadId)
import           Prelude               hiding (error, log)
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Log.FastLogger

{-# ANN module
          ("HLint: ignore Avoid restricted function" :: String)
        #-}

log :: Text -> IO ()
log msg = do
  logSet <- readIORef _logSet
  threadId <- myThreadId
  let prefix = "[" ++ tshow threadId ++ "] "
  pushLogStr logSet $
    toLogStr prefix ++ toLogStr (T.replace "\n" ("\n" ++ prefix) msg) ++ "\n"

_logSet :: IORef LoggerSet
{-# NOINLINE _logSet #-}
_logSet =
  unsafePerformIO $ newIORef =<< newStdoutLoggerSet 0 -- bufsize == 0, to always flush
