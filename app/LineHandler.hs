module LineHandler
       ( LineHandler(..) )
       where

import qualified IrcParser as I

data LineHandler = LineHandler (I.IrcLine -> IO (Maybe I.IrcCommand, LineHandler))
