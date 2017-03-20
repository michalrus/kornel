import System.FilePath.Glob (glob)
import Language.Haskell.HLint
import System.Exit

main :: IO ()
main = do
  putStrLn ""
  hints <- glob "**/*.hs" >>= hlint
  if null hints then exitSuccess else exitFailure
