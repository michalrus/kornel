import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)
import System.FilePath.Glob (glob)

main :: IO ()
main = do
  putStrLn ""
  hints <- glob "**/*.hs" >>= hlint
  unless (null hints) exitFailure
