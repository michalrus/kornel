{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as L8
import Foreign.C.Error
import GHC.IO.Exception
import HIndent
import HIndent.Types
import Language.Haskell.Exts hiding (Style, style)
import qualified System.Directory as IO
import System.Exit
import System.FilePath.Glob (glob)
import qualified System.IO as IO

main :: IO ()
main = do
  putStrLn ""
  paths <- glob "**/*.hs"
  putStrLn $ "paths == " ++ show paths
  results <- mapM formatE paths
  if all id results
    then exitSuccess
    else exitFailure

formatE :: FilePath -> IO Bool
formatE filepath =
  format defaultConfig defaultExtensions filepath >>
  return True `catch` \(e :: SomeException) ->
    IO.hPutStrLn IO.stderr (filepath ++ ": " ++ show e) >> return False

format :: Config -> [Extension] -> FilePath -> IO ()
format style exts filepath = do
  text <- S.readFile filepath
  case reformat style (Just exts) text of
    Left e -> error (filepath ++ ": " ++ e)
    Right out ->
      unless (L8.fromStrict text == S.toLazyByteString out) $ do
        tmpDir <- IO.getTemporaryDirectory
        (fp, h) <- IO.openTempFile tmpDir "hindent.hs"
        L8.hPutStr h (S.toLazyByteString out)
        IO.hFlush h
        IO.hClose h
        let exdev e =
              if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                then IO.copyFile fp filepath >> IO.removeFile fp
                else throw e
        IO.copyPermissions filepath fp
        IO.renameFile fp filepath `catch` exdev
